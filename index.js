'use strict'

// manger-http - cache feeds

module.exports = exports = MangerService

const HttpHash = require('http-hash')
const Negotiator = require('negotiator')
const StringDecoder = require('string_decoder').StringDecoder
const assert = require('assert')
const fs = require('fs')
const http = require('http')
const httpMethods = require('http-methods/method')
const manger = require('manger')
const mkdirp = require('mkdirp')
const path = require('path')
const url = require('url')
const util = require('util')
const zlib = require('zlib')

function nop () {}

// Measure time for log levels below error, which would be 50 in bunyan.
const debugging = parseInt(process.env.NODE_LOG_LEVEL, 10) < 50
const time = debugging ? process.hrtime : nop
const ns = (() => {
  return debugging ? (t) => {
    return t[0] * 1e9 + t[1]
  } : nop
})()

function headers (len, lat, enc) {
  const headers = {
    'Cache-Control': 'max-age=' + 86400,
    'Content-Type': 'application/json; charset=utf-8',
    'Content-Length': len
  }
  if (lat) {
    headers['Latency'] = lat
  }
  if (enc) {
    headers['Content-Encoding'] = enc
  }
  return headers
}

function getGz (req) {
  const neg = new Negotiator(req)
  return neg.preferredEncoding(['gzip', 'identity']) === 'gzip'
}

// A general responder for buffered payloads that applies gzip compression
// if requested.
//
// - req IncomingMessage The request.
// - res ServerResponse The response.
// - statusCode Number The HTTP status code.
// - payload Buffer | String The JSON payload.
// - time Array | void The hi-res real time tuple of when the request hit
// - log The logger to use.
function respond (req, res, statusCode, payload, time, log) {
  assert(!res.finished, 'cannot respond more than once')
  function onfinish () {
    res.removeListener('close', onclose)
    res.removeListener('finish', onfinish)
    res = null
    req = null
    log = null
  }
  function onclose () {
    log.warn('connection terminated: ' + req.url)
    onfinish()
  }
  const gz = getGz(req)
  function lat () {
    if (time instanceof Array) {
      return latency(time, log)
    }
  }
  if (gz) {
    zlib.gzip(payload, function gzipHandler (er, zipped) {
      if (!res) return
      const h = headers(zipped.length, lat(), 'gzip')
      res.writeHead(statusCode, h)
      res.end(zipped)
    })
  } else {
    const len = Buffer.byteLength(payload, 'utf8')
    const h = headers(len, lat())
    res.writeHead(statusCode, h)
    res.end(payload)
  }
  res.on('close', onclose)
  res.on('finish', onfinish)
}

// Assess specified error by its message returning `true`, if it might be OK to
// continue after this error has occured.
function ok (er) {
  var ok = false
  const whitelist = RegExp([
    'ECONNREFUSED',
    'ECONNRESET',
    'ENOTFOUND',
    'ETIMEDOUT',
    'already updating',
    'client error',
    'not deleted',
    'parse error',
    'quaint HTTP status',
    'query error',
    'request error',
    'socket hang up'
  ].join('|'), 'i')
  if (er) {
    const msg = er.message
    if (msg) ok = msg.match(whitelist) !== null
  }
  return ok
}

function query (s, req, opts, cb) {
  const t = time()

  var queries = manger.queries()
  var buf = ''

  function onError (error) {
    const er = new Error(error.message)
    er.url = error.url

    if (ok(er)) {
      opts.log.error({ err: er, url: er.url })
    } else {
      req.resume()
      queries.end()
      s.end(er)
    }
  }
  function onClose () {
    const er = new Error('client error: connection terminated')
    onError(er)
  }
  function onData (chunk) {
    buf += chunk
  }
  function onQuery (q) {
    opts.log.debug(q, 'query')
  }

  function onEnd (er) {
    req.removeListener('error', onError)
    req.removeListener('close', onClose)
    req.unpipe(queries)

    assert(queries._readableState.ended, 'queries should always end')
    assert(queries._writableState.ended, 'queries should always end')
    queries.removeListener('error', onError)
    queries.removeListener('warning', onError)
    queries.removeListener('data', onQuery)
    queries.unpipe(s)

    assert(s._readableState.ended, 'stream should always end')
    assert(s._writableState.ended, 'stream should always end')
    s.removeListener('error', onError)
    s.removeListener('data', onData)
    s.removeListener('end', onEnd)

    const sc = er ? er.statusCode || 404 : 200
    cb(er, sc, buf, t)

    // Making sure we'd notice sloppy event handling.
    s = req = opts = cb = queries = buf = null
  }

  req.on('error', onError)
  req.on('close', onClose)

  queries.on('error', onError)
  queries.on('warning', onError)
  queries.on('data', onQuery)

  s.on('error', onError)
  s.on('data', onData)
  s.on('end', onEnd)

  req.pipe(queries)
  queries.pipe(s)
}

function feeds (req, res, opts, cb) {
  const s = opts.manger.feeds()
  query(s, req, opts, cb)
}

function urlFromParams (params) {
  const str = params.url
  return typeof str === 'string' ? unescape(str) : null
}

function crash (er, log) {
  if (log && typeof log.fatal === 'function') log.fatal(er)
  process.nextTick(() => { throw er })
}

// Handle requests for a single feed or entries of a single feed, never returning
// an error, but a 200 and an empty result array instead. I'm not crazy about this!
function single (s, req, res, opts, cb) {
  const t = time()
  var buf = ''

  function onreadable () {
    var chunk
    while ((chunk = s.read()) !== null) {
      buf += chunk
    }
  }
  function onerror (er) {
    const error = new Error(er.message)
    if (!ok(error)) return crash(error, opts.log)
    opts.log.warn(error)
  }
  function onend (er) {
    assert(s._readableState.ended, 'stream should always end')
    assert(s._writableState.ended, 'stream should always end')

    s.removeListener('end', onend)
    s.removeListener('error', onerror)
    s.removeListener('readable', onreadable)

    cb(null, 200, buf, t)

    s = req = res = opts = buf = cb = null
  }
  s.on('error', onerror)
  s.on('end', onend)
  s.on('readable', onreadable)

  const params = opts.params
  const uri = urlFromParams(params)
  const q = manger.query(uri)

  if (q === null) {
    opts.log.warn(opts.params, 'awry query')
  }

  s.end(q)
}

function feed (req, res, opts, cb) {
  const s = opts.manger.feeds()
  single(s, req, res, opts, cb)
}

function entriesOfFeed (req, res, opts, cb) {
  const s = opts.manger.entries()
  single(s, req, res, opts, cb)
}

function deleteFeed (req, res, opts, cb) {
  const t = time()
  const params = opts.params
  const uri = urlFromParams(params)
  const cache = opts.manger
  cache.remove(uri, function cacheRemoveHandler (er) {
    var sc = 200
    var buf
    var error
    if (er) {
      const problem = 'not deleted'
      var reason
      if (er.notFound) {
        reason = uri + ' not cached'
        sc = 404
      } else {
        sc = 500
      }
      buf = JSON.stringify({
        error: problem,
        reason: reason
      })
      error = new Error(problem + ': ' + reason)
    } else {
      buf = JSON.stringify({
        ok: true,
        id: uri
      })
    }
    cb(error, sc, buf, t)
  })
}

function entries (req, res, opts, cb) {
  const s = opts.manger.entries()
  query(s, req, opts, cb)
}

function factor (count) {
  return Math.max(Math.min(Math.round(count * 0.1), 500), 1)
}

const OK = JSON.stringify({
  ok: true
})

const NOT_OK = JSON.stringify({
  ok: false
})

const ONE_DAY = 1.15741e8

// Update all feeds currently in the cache, ordered by rank. This uses a stream as a
// serial queue to update the feeds in order. Use this function by binding it to
// a `MangerService` object--it is the only stateful route of this service.
function update (req, res, opts, cb) {
  const now = Date.now()
  const then = this.updating
  const locked = typeof then === 'number' ? now - then < ONE_DAY : false

  if (locked) {
    const er = new Error('update error: already updating')
    return cb(er, 423, NOT_OK)
  }

  // We are using a timestamp, instead of a plain boolean, for our updating flag to
  // limit the duration, currently one day, of the lock.
  this.updating = now

  var me = this

  opts.manger.flushCounter(function onFlushCounter (er, feedCount) {
    if (er) {
      const failure = 'not updated'
      const reason = er.message
      const error = new Error(`${failure}: ${reason}`)
      const payload = JSON.stringify({
        error: failure,
        reason: reason
      })
      cb(error, 500, payload)
      cb = null

      me.updating = null
      me = null
      opts = null
      return
    }
    cb(null, 202, OK)
    cb = null

    if (feedCount === 0) {
      opts.log.warn('empty cache')
      me.updating = null
      return
    }

    const x = 1
    var s = opts.manger.update(x)
    const t = time()
    var count = 0
    function ondata (feed) {
      opts.log.debug('updated', feed.feed)
      count++
    }
    var errors = []
    function onerror (er) {
      if (ok(er)) {
        opts.log.warn({ err: er, url: er.url }, 'update')
        errors.push(er)
      } else {
        const failure = 'update error'
        const reason = er.message
        const error = new Error(failure + ': ' + reason)
        crash(error, opts.log)
      }
    }
    function onend () {
      s.removeListener('data', ondata)
      s.removeListener('error', onerror)
      s.removeListener('end', onend)
      s = null
      if (count > 0) {
        var lat
        if (t) lat = ns(time(t))
        const secs = lat ? (lat / 1e9).toFixed(2) : ''
        const info = {
          feeds: count,
          streams: x,
          latency: secs
        }
        opts.log.info(info, 'updated')
      //
      // If we have received 'ENOTFOUND' for all feeds we've been trying to update,
      // tolerating maximally five of five, we can assume that we have no outbound
      // Internet connectivity--although we've just been counting errors, without
      // relating them to the feeds. To get the attention of an operator we
      // deliberately opt to crash.
      //
      } else if (feedCount > 5 && errors.filter((er) => {
        return er.code === 'ENOTFOUND'
      }).length >= feedCount) {
        const er = new Error('no connection')
        return crash(er, opts.log)
      } else {
        opts.log.warn('no updates')
      }
      req = null

      me.updating = null
      opts = null
    }
    s.on('data', ondata)
    s.on('error', onerror)
    s.on('end', onend)
  })
}

function latency (t, log) {
  const lat = ns(time(t))
  const limit = 21e6
  if (lat > limit) {
    log.warn({ ms: (lat / 1e6).toFixed(2) }, 'latency')
  }
  return lat
}

function urls (readable, opts, cb) {
  assert(cb, 'callback required')
  const sc = 200
  const t = time()
  var urls = []
  function onend (er) {
    if (!urls) return
    readable.removeListener('data', ondata)
    readable.removeListener('error', onerror)
    readable.removeListener('end', onend)
    const payload = JSON.stringify(urls)
    urls = null
    cb(null, sc, payload, t)
  }
  function onerror (er) {
    onend(er)
  }
  var decoder
  function decode (chunk) {
    if (!decoder) decoder = new StringDecoder('utf8')
    return decoder.write(chunk)
  }
  function ondata (chunk) {
    if (!urls) return
    if (chunk instanceof Buffer) {
      chunk = decode(chunk)
    }
    urls.push(chunk)
  }
  readable.on('data', ondata)
  readable.on('error', onerror)
  readable.on('end', onend)
}

function list (req, res, opts, cb) {
  const s = opts.manger.list()
  urls(s, opts, cb)
}

function root (req, res, opts, cb) {
  const payload = JSON.stringify({
    name: 'manger',
    version: opts.version
  })
  cb(null, 200, payload)
}

function ranks (req, res, opts, cb) {
  const s = opts.manger.ranks()
  urls(s, opts, cb)
}

function resetRanks (req, res, opts, cb) {
  cb(null, 202, OK)
  const cache = opts.manger
  cache.resetRanks(function resetRanksHandler (er) {
    if (er) {
      const error = new Error('could not reset ranks: ' + er.message)
      opts.log.warn(error)
    }
  })
}

function flushCounter (req, res, opts, cb) {
  cb(null, 202, OK)
  const cache = opts.manger
  cache.flushCounter(function flushCounterHandler (er, count) {
    if (er) opts.log.warn(er)
  })
}

function version () {
  const p = path.join(__dirname, 'package.json')
  const data = fs.readFileSync(p)
  const pkg = JSON.parse(data)
  return pkg.version
}

function defaults (opts) {
  opts = opts || Object.create(null)
  opts.location = opts.location || '/tmp/manger-http'
  opts.port = opts.port || 8384
  opts.log = (() => {
    if (opts.log) return opts.log
    return {
      fatal: nop, error: nop, warn: nop, info: nop, debug: nop, trace: nop
    }
  })()
  opts.ttl = opts.ttl || ONE_DAY
  opts.cacheSize = opts.cacheSize || 16 * 1024 * 1024
  return opts
}

function MangerService (opts) {
  if (!(this instanceof MangerService)) return new MangerService(opts)

  opts = defaults(opts)
  util._extend(this, opts)

  this.hash = HttpHash()
  this.version = version()

  this.manger = null
  this.server = null

  mkdirp.sync(this.location)
}

function ReqOpts (log, manger, params, splat, version) {
  this.log = log
  this.manger = manger
  this.params = params
  this.splat = splat
  this.version = version
}

MangerService.prototype.handleRequest = function (req, res, cb) {
  if (typeof cb !== 'function') {
    throw new Error('callback required to handle request')
  }

  var pathname = url.parse(req.url).pathname

  var route = this.hash.get(pathname)
  if (route.handler === null) {
    const er = new Error('not found')
    er.statusCode = 404
    return cb(er)
  }

  const opts = new ReqOpts(
    this.log,
    this.manger,
    route.params,
    route.splat,
    this.version
  )

  return route.handler(req, res, opts, cb)
}

function errorHandler (er) {
  if (ok(er)) {
    this.log.warn(er.message)
  } else {
    const failure = 'fatal error'
    const reason = er.message
    const error = new Error(`${failure}: ${reason}`)
    crash(error, this.log)
  }
}

// TODO: Support HEAD

MangerService.prototype.setRoutes = function () {
  const hash = this.hash
  function set (name, handler) {
    if (handler && typeof handler === 'object') {
      handler = httpMethods(handler)
    }
    hash.set(name, handler)
  }

  set('/', root)

  set('/entries', {
    POST: entries
  })
  set('/entries/:url', {
    GET: entriesOfFeed
  })
  set('/feeds', {
    GET: list,
    POST: feeds,
    PUT: update.bind(this)
  })
  set('/feed/:url', {
    GET: feed,
    DELETE: deleteFeed
  })
  set('/ranks', {
    GET: ranks,
    DELETE: resetRanks,
    PUT: flushCounter
  })
}

MangerService.prototype.start = function (cb) {
  cb = cb || nop

  const log = this.log

  const info = {
    version: this.version,
    location: this.location,
    size: this.cachSize
  }
  log.info(info, 'start')

  const cache = manger(this.location, {
    cacheSize: this.cacheSize
  })
  this.errorHandler = errorHandler.bind(this)
  cache.on('error', this.errorHandler)
  this.manger = cache

  this.setRoutes()

  const me = this

  function onrequest (req, res) {
    log.info({ method: req.method, url: req.url }, 'request')

    function terminate (er, statusCode, payload, time) {
      if (er) {
        const payloads = {
          404: () => {
            log.warn({ method: req.method, url: req.url }, 'no route')
            const reason = req.url + ' is no route'
            statusCode = 404
            payload = JSON.stringify({
              error: 'not found',
              reason: reason
            })
          },
          405: () => {
            log.warn({ method: req.method, url: req.url }, 'not allowed')
            const reason = req.method + ' ' + req.url + ' is undefined'
            statusCode = 405
            payload = JSON.stringify({
              error: 'method not allowed',
              reason: reason
            })
          }
        }
        if (er.statusCode in payloads) {
          payloads[er.statusCode]()
        } else if (ok(er)) {
          if (!payload) {
            return crash(er, log)
          }
          log.warn(er)
        } else {
          return crash(er, log)
        }
      }
      respond(req, res, statusCode, payload, time, log)

      req = null
      res = null
    }

    me.handleRequest(req, res, terminate)
  }

  const server = http.createServer(onrequest)
  const port = this.port

  server.listen(port, function (er) {
    const info = {
      port: port,
      sockets: http.globalAgent.maxSockets
    }
    log.info(info, 'listen')
    cb(er)
  })

  server.on('clientError', function (er, socket) {
    // Logging in the 'close' callback, because I've seen the call stack being
    // exceeded in bunyan.js, in line 958 at this moment, suggesting a race
    // condition in the error handler. To circumvent this, we also check if the
    // socket has been destroyed already, before we try to close it.
    socket.once('close', () => { log.warn(er) })
    if (!socket.destroyed) {
      socket.end('HTTP/1.1 400 Bad Request\r\n\r\n')
    }
  })

  this.server = server
}

// Warning: Tests only! Restarting is undefined.
MangerService.prototype.stop = function (cb) {
  cb = cb || nop
  const me = this
  function closeServer () {
    const server = me.server
    server ? server.close(next) : next()
  }
  function closeDB () {
    const db = me.manger ? me.manger.db : null
    db ? db.close(next) : next()
  }
  const tasks = [closeServer, closeDB]
  function next (er) {
    if (er) {
      const error = new Error('stop error: ' + er.message)
      me.log.warn(error)
    }
    const f = tasks.shift()
    if (f) {
      f()
    } else {
      if (me.errorHandler) {
        me.manger.removeListener('error', me.errorHandler)
      }
      me.manger = null
      me.server = null
      cb()
    }
  }
  next()
}

if (parseInt(process.env.TAP, 10) === 1) {
  exports.MangerService = MangerService
  exports.defaults = defaults
  exports.factor = factor
  exports.nop = nop
  exports.ok = ok
}
