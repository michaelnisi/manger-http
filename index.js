'use strict'

// manger-http - serve podcast feeds with persistent caching

module.exports = exports = MangerService

const HttpHash = require('http-hash')
const Negotiator = require('negotiator')
const assert = require('assert')
const etag = require('etag')
const fs = require('fs')
const http = require('http')
const httpMethods = require('http-methods/method')
const manger = require('manger')
const mkdirp = require('mkdirp')
const path = require('path')
const querystring = require('querystring')
const url = require('url')
const zlib = require('zlib')
const { StringDecoder } = require('string_decoder')
const { createLogger } = require('./lib/log')
const { issue, Purge } = require('./lib/proxy')

function nop () {}

// Returns summed nanoseconds from a [seconds, nanoseconds] tuple Array.
function nanoseconds (t) {
  return t[0] * 1e9 + t[1]
}

// Returns HTTP headers for content-length, latency, encoding, and etag.
function headers (len, lat, enc, tag) {
  assert(arguments.length === 4)

  const headers = {
    'Cache-Control': 'max-age=' + 86400,
    'Content-Length': len,
    'Content-Type': 'application/json; charset=utf-8',
    'Surrogate-Control': 'max-age=' + 21600
  }

  if (lat) {
    headers['Backend-Latency'] = lat
  }

  if (enc) {
    headers['Content-Encoding'] = enc
  }

  if (tag) {
    headers['ETag'] = tag
  }

  return headers
}

// Returns true if the request prefers gzip encoding.
function getGz (req) {
  const neg = new Negotiator(req)

  return neg.preferredEncoding(['gzip', 'identity']) === 'gzip'
}

// Returns true if the client's ETag (sent with If-None-Match) matches tag.
function isMatching (headers, tag) {
  const client = headers['If-None-Match'] || headers['if-none-match']

  return client === tag
}

// Responds with buffered payload, applying gzip compression if preferred,
// flushing request buffers when done.
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
    req.resume() // making sure buffers are flushed in all cases

    res.removeListener('close', onclose)
    res.removeListener('finish', onfinish)
  }

  function onclose () {
    log.warn('connection terminated: ' + req.url)
    onfinish()
  }

  // Ends reponse with headers, body, and etag.
  function end (headers, body, tag) {
    if (isMatching(req.headers, tag)) {
      res.writeHead(304, headers)
      res.end('')
    } else if (req.method === 'HEAD') {
      res.writeHead(statusCode, headers)
      res.end('')
    } else {
      res.writeHead(statusCode, headers)
      res.end(body)
    }
  }

  const gz = getGz(req)

  if (gz) {
    zlib.gzip(payload, (er, zipped) => {
      if (!res) return

      const tag = etag(zipped)
      const h = headers(zipped.length, latency(time, log), 'gzip', tag)

      end(h, zipped, tag)
    })
  } else {
    const len = Buffer.byteLength(payload, 'utf8')
    const tag = etag(payload)
    const h = headers(len, latency(time, log), null, tag)

    end(h, payload, tag)
  }

  res.on('close', onclose)
  res.on('finish', onfinish)
}

// Whitelists errors we tolerate without crashing.
const whitelist = RegExp([
  'ECONNREFUSED',
  'ECONNRESET',
  'ENOTFOUND',
  'ETIMEDOUT',
  'certificate',
  'client error',
  'currently updating',
  'not deleted',
  'parse error',
  'quaint HTTP status',
  'query error',
  'request error',
  'socket hang up',
  'too many redirects',
  'update error: write EPIPE'
].join('|'), 'i')

// Returns true if the error is on the whitelist.
function ok (er) {
  let ok = false

  if (er) {
    const msg = er.message
    if (msg) ok = msg.match(whitelist) !== null
  }

  return ok
}

// Pipes request, via Queries, into Transform s and applies cb with result.
function query (s, req, opts, cb) {
  const t = process.hrtime()
  const queries = manger.Queries()

  // Result

  let err = null
  let buf = ''

  // Request

  const onRequestError = (er) => {
    err = new Error(er.message)
  }

  const onRequestClose = () => {
    const er = new Error('client error: connection terminated')

    onRequestError(er)
  }

  // Queries

  const onQueriesError = (er) => {
    err = new Error(er.message)
  }

  const onQueriesWarning = (er) => {
    // What’s the point of emitting warnings if we are handling them like errors?
    err = new Error(er.message)
  }

  const onQueriesData = (q) => { opts.log.debug(q, 'query') }

  // Transform

  const onStreamError = (er) => {
    err = new Error(er.message)
  }

  const onStreamData = (chunk) => { buf += chunk }

  const onStreamEnd = () => {
    req.removeListener('error', onRequestError)
    req.removeListener('close', onRequestClose)
    req.unpipe(queries)

    queries.removeListener('error', onQueriesError)
    queries.removeListener('warning', onQueriesWarning)
    queries.removeListener('data', onQueriesData)
    queries.unpipe(s)
    queries.resume()

    s.removeListener('error', onStreamError)
    s.removeListener('data', onStreamData)
    s.removeListener('end', onStreamEnd)
    s.resume()

    const sc = err ? err.statusCode || 404 : 200

    if (cb) cb(err, sc, buf, t)
  }

  req.on('error', onRequestError)
  req.on('close', onRequestClose)

  queries.on('error', onQueriesError)
  queries.on('warning', onQueriesWarning)
  queries.on('data', onQueriesData)

  s.on('error', onStreamError)
  s.on('data', onStreamData)
  s.on('end', onStreamEnd)

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

// Handles requests for single feeds or entries of single feeds. The purpose
// of these extra routes is to enhance external caching by using these clearcut
// GET requests for singular things, which are quite common use cases with our
// clients.
function single (s, req, res, opts, cb) {
  const t = process.hrtime()

  let err = null
  let buf = ''

  function onreadable () {
    let chunk

    while ((chunk = s.read()) !== null) {
      buf += chunk
    }
  }

  function onerror (er) {
    err = new Error(`${req.url}: ${String(er)}`)
  }

  function onend () {
    s.removeListener('end', onend)
    s.removeListener('error', onerror)
    s.removeListener('readable', onreadable)

    s.resume()

    if (cb) cb(err, 200, buf, t)
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
  const t = process.hrtime()
  const params = opts.params
  const uri = urlFromParams(params)
  const cache = opts.manger

  cache.remove(uri, function cacheRemoveHandler (er) {
    let sc = 200
    let buf
    let error

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
      buf = JSON.stringify({ ok: true, id: uri })
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

const OK = JSON.stringify({ ok: true })

const NOT_OK = JSON.stringify({ ok: false })

// Updates the cache, where `this` is a `MangerService` object.
function update (req, res, opts, cb) {
  const now = Date.now()
  const then = this.updating
  const limit = opts.maxUpdates

  const locked = typeof then === 'number' ? now - then < limit : false

  if (locked) {
    const er = new Error('locked: currently updating')

    return cb(er, 423, NOT_OK)
  }

  // We are using a timestamp, instead of a plain boolean, for our updating flag to
  // limit the duration, currently one day, of the lock.
  this.updating = now

  const cache = opts.manger
  const log = opts.log

  cache.flushCounter((er, feedCount) => {
    if (er) {
      const failure = 'not updated'
      const reason = er.message
      const error = new Error(`${failure}: ${reason}`)
      const payload = JSON.stringify({ error: failure, reason: reason })

      this.updating = null

      return cb ? cb(error, 500, payload) : null
    }

    if (cb) cb(null, 202, OK)

    if (feedCount === 0) {
      log.warn('empty cache')
      return (this.updating = null)
    }

    const t = process.hrtime()

    const feedsPerStream = 64
    const x = Math.min(Math.ceil(feedCount / feedsPerStream), 16)
    const s = cache.update(x)

    let count = 0

    function ondata (feed) {
      log.debug('updated', feed.url)

      issue(new Purge('/feed', feed.url))
      issue(new Purge('/entries', feed.url))

      count++
    }

    const errors = []

    function onerror (er) {
      if (ok(er)) {
        log.warn({ err: er, url: er.url }, 'update')
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

      if (count > 0) {
        const info = { feeds: count, streams: x, ms: latency(t) }
        opts.log.warn(info, 'updated')
      } else if (feedCount > 5 && errors.filter((er) => {
        // If we have received 'ENOTFOUND' for all feeds, we've been trying to
        // update, tolerating maximally five of five, we can assume that we
        // have no outbound Internet connectivity -- although we've just been
        // counting errors, without relating them to the feeds. To get the
        // attention of an operator we deliberately opt to crash.

        return er.code === 'ENOTFOUND'
      }).length >= feedCount) {
        const er = new Error('no connection')

        return crash(er, opts.log)
      } else {
        opts.log.warn('no updates')
      }

      this.updating = null
    }

    s.on('data', ondata)
    s.on('error', onerror)
    s.on('end', onend)
  })
}

// Returns latency in milliseconds and logs if it exceeds 20 ms. Having no
// control over how long outbound HTTP requests for fetching unknown feeds
// take, this isn’t a rare occurrence. Trying to differentiate using log
// levels, warning above one and a half seconds. The goal is to prevent
// flooding our production logs, which are at level 40 (warn).
function latency (t, log) {
  if (!Array.isArray(t)) {
    return
  }

  const lat = nanoseconds(process.hrtime(t))

  if (lat > 2e7) {
    const ms = (lat / 1e6).toFixed(2)

    if (log) {
      if (ms > 1500) {
        log.warn({ ms: ms }, 'latency')
      } else {
        log.info({ ms: ms }, 'latency')
      }
    }

    return ms
  }
}

function urls (readable, opts, cb) {
  assert(cb, 'callback required')

  const sc = 200
  const t = process.hrtime()
  const urls = []

  function onend (er) {
    readable.removeListener('data', ondata)
    readable.removeListener('error', onerror)
    readable.removeListener('end', onend)

    const payload = JSON.stringify(urls)

    if (cb) cb(null, sc, payload, t)
  }

  function onerror (er) {
    onend(er)
  }

  const decoder = new StringDecoder('utf8')

  function decode (chunk) {
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

  return cb ? cb(null, 200, payload) : null
}

function limit (query) {
  if (!query) return

  const limit = Number(querystring.parse(query).limit)

  return limit > 0 ? limit : null
}

function ranks (req, res, opts, cb) {
  const query = url.parse(req.url).query
  const s = opts.manger.ranks(limit(query))

  return urls(s, opts, cb)
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
  const log = opts.log

  cache.flushCounter((er, count) => {
    if (er) log.warn(er)
    log.debug({ count: count }, 'flushed')
  })
}

function version () {
  const p = path.join(__dirname, 'package.json')
  const data = fs.readFileSync(p)
  const pkg = JSON.parse(data)

  return pkg.version
}

// Returns options completed with defaults.
function defaults (opts) {
  opts = opts || Object.create(null)
  opts.location = opts.location || '/tmp/manger-http'
  opts.port = opts.port || 8384
  opts.log = createLogger(opts.log)
  opts.ttl = opts.ttl || 1.15741e8
  opts.cacheSize = opts.cacheSize || 16 * 1024 * 1024

  return opts
}

// Creates a new manger service with options.
function MangerService (opts) {
  if (!(this instanceof MangerService)) return new MangerService(opts)

  opts = defaults(opts)
  Object.assign(this, opts)

  this.hash = HttpHash()
  this.version = version()

  this.manger = null
  this.server = null

  mkdirp.sync(this.location)
}

// Additional request state.
function ReqOpts (log, manger, params, splat, version) {
  this.log = log
  this.manger = manger
  this.params = params
  this.splat = splat
  this.version = version
}

// Handles request and response passing a callback into the route handler.
MangerService.prototype.handleRequest = function (req, res, cb) {
  if (typeof cb !== 'function') {
    throw new Error('callback required to handle request')
  }

  const pathname = url.parse(req.url).pathname

  const route = this.hash.get(pathname)

  if (route.handler === null) {
    const er = new Error('not found')
    er.statusCode = 404

    return cb ? cb(er) : null
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

// Debug logs cache hits.
function hitHandler (qry) {
  this.log.debug(qry, 'hit')
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

MangerService.prototype.setRoutes = function () {
  const hash = this.hash

  function set (name, handler) {
    if (handler && typeof handler === 'object') {
      handler = httpMethods(handler)
    }
    hash.set(name, handler)
  }

  set('/', {
    GET: root,
    HEAD: root
  })

  set('/entries', {
    HEAD: entries,
    POST: entries
  })

  set('/entries/:url', {
    GET: entriesOfFeed,
    HEAD: entriesOfFeed
  })

  set('/feeds', {
    GET: list,
    HEAD: list,
    POST: feeds,
    PUT: update.bind(this)
  })

  set('/feed/:url', {
    DELETE: deleteFeed,
    GET: feed,
    HEAD: feed
  })

  set('/ranks', {
    DELETE: resetRanks,
    GET: ranks,
    HEAD: ranks,
    PUT: flushCounter
  })
}

MangerService.prototype.start = function (cb) {
  const log = this.log

  const info = {
    version: this.version,
    location: this.location,
    cacheSize: this.cacheSize,
    maxUpdates: this.maxUpdates
  }

  log.info(info, 'start')

  const cache = manger(this.location, {
    cacheSize: this.cacheSize,
    isEntry: (entry) => {
      if (entry.enclosure) return true
      log.info(entry.url, 'invalid entry')
    },
    isFeed: (feed) => {
      return true
    }
  })

  this.errorHandler = errorHandler.bind(this)

  cache.on('error', this.errorHandler)

  this.hitHandler = hitHandler.bind(this)

  cache.on('hit', this.hitHandler)

  this.manger = cache

  this.setRoutes()

  const onrequest = (req, res) => {
    log.info({ method: req.method, url: req.url }, 'request')

    const terminate = (er, statusCode, payload, time) => {
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
    }

    this.handleRequest(req, res, terminate)
  }

  const server = http.createServer(onrequest)
  const port = this.port

  server.listen(port, (er) => {
    const info = {
      port: port,
      sockets: http.globalAgent.maxSockets
    }

    log.info(info, 'listen')
    if (cb) cb(er)
  })

  server.on('clientError', (er, socket) => {
    //
    // Logging in the 'close' callback, because I've seen the call stack being
    // exceeded in bunyan.js, in line 958 at this moment, suggesting a race
    // condition in the error handler. To circumvent this, we also check if the
    // socket has been destroyed already, before we try to close it.
    //
    socket.once('close', () => { log.warn(er) })
    if (!socket.destroyed) {
      socket.end('HTTP/1.1 400 Bad Request\r\n\r\n')
    }
  })

  this.server = server
}

// Warning: Tests only! Restarting is undefined.
MangerService.prototype.stop = function (cb) {
  const closeServer = () => {
    const server = this.server

    server ? server.close(next) : next()
  }

  const closeDB = () => {
    const db = this.manger ? this.manger.db : null

    db ? db.close(next) : next()
  }

  const tasks = [closeServer, closeDB]

  const next = (er) => {
    if (er) {
      const error = new Error('stop error: ' + er.message)

      this.log.warn(error)
    }

    const f = tasks.shift()

    if (f) {
      f()
    } else {
      this.manger.removeAllListeners()
      if (cb) cb()
    }
  }

  next()
}

if (process.mainModule.filename.match(/test/) !== null) {
  exports.MangerService = MangerService
  exports.defaults = defaults
  exports.factor = factor
  exports.nop = nop
  exports.ok = ok
}
