// manger-http - cache feeds

// TODO: Handle HEAD requests

module.exports = exports = MangerService

var HttpHashRouter = require('http-hash-router')
var Negotiator = require('negotiator')
var StringDecoder = require('string_decoder').StringDecoder
var assert = require('assert')
var fs = require('fs')
var http = require('http')
var manger = require('manger')
var mkdirp = require('mkdirp')
var net = require('net')
var path = require('path')
var repl = require('repl')
var util = require('util')
var zlib = require('zlib')

function nop () {}

var debugging = parseInt(process.env.NODE_DEBUG, 10) === 1
var debug = (function () {
  return debugging ? function (o) {
    console.error('** manger-http: %s', util.inspect(o))
  } : nop
})()
var time = debugging ? process.hrtime : nop
var ns = (function () {
  return debugging ? function (t) {
    return t[0] * 1e9 + t[1]
  } : nop
})()

function headers (len, lat, enc) {
  var headers = {
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
  var gz = false
  var neg = new Negotiator(req)
  gz = neg.preferredEncoding(['gzip', 'identity']) === 'gzip'
  return gz
}

// TODO: Move into separate module

// A general responder for buffered payloads that applies gzip compression
// if requested.
//
// - req IncomingMessage The request
// - res ServerResponse The response
// - statusCode Number The HTTP status code
// - payload Buffer | String The JSON payload
// - time Array | void The hi-res real time tuple of when the request hit
// - opts ReqOpts The options objects // TODO: Replace with log
function respond (req, res, statusCode, payload, time, opts) {
  assert(!res.finished, 'attempted to respond more than once')
  function onfinish () {
    res.removeListener('close', onclose)
    res.removeListener('finish', onfinish)
    res = null
    req = null
    opts = null
  }
  function onclose () {
    opts.log.warn('connection terminated: ' + req.url)
    onfinish()
  }
  var gz = getGz(req)
  function lat () {
    if (time instanceof Array) {
      return latency(time, opts.log)
    }
  }
  if (gz) {
    zlib.gzip(payload, function (er, zipped) {
      if (!res) return
      var h = headers(zipped.length, lat(), 'gzip')
      res.writeHead(statusCode, h)
      res.end(zipped)
    })
  } else {
    var len = Buffer.byteLength(payload, 'utf8')
    var h = headers(len, lat())
    res.writeHead(statusCode, h)
    res.end(payload)
  }
  res.on('close', onclose)
  res.on('finish', onfinish)
}

function ok (er) {
  var ok = false
  var whitelist = RegExp([
    'client error',
    'invalid query',
    'not deleted',
    'parse error',
    'quaint HTTP status',
    'query error',
    'request error'
  ].join('|'))
  if (er) {
    var msg = er.message
    if (msg) ok = msg.match(whitelist) !== null
  }
  return ok
}

function query (s, req, opts, cb) {
  var t = time()
  var queries = manger.queries()
  var buf = ''
  function onend (er) {
    if (!queries) return

    s.removeListener('data', ondata)
    s.removeListener('end', onend)
    s.removeListener('error', onerror)
    queries.removeListener('error', onerror)
    req.removeListener('close', onclose)
    req.unpipe(queries)
    queries.unpipe(s)
    queries = null

    var sc = er ? er.statusCode || 404 : 200
    cb(er, sc, buf, t)
    buf = null
  }
  function onerror (er) {
    var error = new Error(er.message)
    if (!ok(er)) {
      onend(error)
    } else {
      opts.log.warn(error)
    }
  }
  function onclose () {
    var er = new Error('client error: connection terminated')
    onerror(er)
  }
  function ondata (chunk) {
    buf += chunk
  }
  s.on('data', ondata)
  s.on('end', onend)
  s.on('error', onerror)
  queries.on('error', onerror)
  req.on('close', onclose)

  req.pipe(queries)
  queries.pipe(s)
}

function feeds (req, res, opts, cb) {
  var s = opts.manger.feeds()
  query(s, req, opts, cb)
}

function urlFromParams (params) {
  var str = params.url
  return typeof str === 'string' ? unescape(str) : null
}

function single (s, req, res, opts, cb) {
  var t = time()
  var buf = ''
  s.on('readable', function () {
    var chunk
    while ((chunk = s.read()) !== null) {
      buf += chunk
    }
  })
  function onerror (er) {
    // TODO: Understand why all these errors occur after `end`
    var error = new Error(er.message)
    onend(error)
  }
  function onend (er) {
    if (!buf) {
      opts.log.warn(er) // still log after end
      return
    }
    s.removeListener('end', onend)
    s.removeListener('error', onerror)
    var sc = er ? er.statusCode || 404 : 200
    cb(er, sc, buf, t)
    buf = null
  }
  s.on('end', onend)
  s.on('error', onerror)

  var params = opts.params
  var uri = urlFromParams(params)
  var q = manger.query(uri)
  s.end(q)
}

function feed (req, res, opts, cb) {
  var s = opts.manger.feeds()
  single(s, req, res, opts, cb)
}

function entriesOfFeed (req, res, opts, cb) {
  var s = opts.manger.entries()
  single(s, req, res, opts, cb)
}

function deleteFeed (req, res, opts, cb) {
  var t = time()
  var params = opts.params
  var uri = urlFromParams(params)
  var cache = opts.manger
  cache.remove(uri, function handler (er) {
    var sc = 200
    var buf
    var error
    if (er) {
      var problem = 'not deleted'
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
  var s = opts.manger.entries()
  query(s, req, opts, cb)
}

function factor (count) {
  return Math.max(Math.min(Math.round(count * 0.1), 500), 1)
}

function plural (n) {
  return n > 1 || n === 0 ? 's' : ''
}

var OK = JSON.stringify({
  ok: true
})

function update (req, res, opts, cb) {
  // TODO: Warn in case of overlapping updates
  opts.manger.flushCounter(function (er, feedCount) {
    if (er) {
      var failure = 'not updated'
      var reason = er.message
      var error = new Error([failure, reason].join([': ']))
      var payload = JSON.stringify({
        error: failure,
        reason: reason
      })
      cb(error, 500, payload)
      cb = null
      opts = null
      return
    }
    cb(null, 202, OK)
    cb = null
    var x = factor(feedCount)
    x = 3
    var s = opts.manger.update(x)
    var t = time()
    var count = 0
    function ondata (chunk) {
      count++
    }
    function onerror (er) {
      if (ok(er)) {
        opts.log.warn(er.message)
      } else {
        var failure = 'update error'
        var reason = er.message
        var error = new Error([failure, reason].join([': ']))
        opts.log.error(error)
        throw error
      }
    }
    function onend () {
      if (!s) return
      s.removeListener('data', ondata)
      s.removeListener('error', onerror)
      s.removeListener('end', onend)
      s = null
      var lat
      if (t) lat = ns(time(t))
      var secs = lat ? ' in ' + (lat / 1e9).toFixed(2) + ' s' : ''
      opts.log.info(count + ' feed' + plural(count) +
        ' updated with ' + x + ' stream' + plural(x) + secs)
      req = null
      opts = null
    }
    s.on('data', ondata)
    s.on('error', onerror)
    s.on('end', onend)
  })
}

function latency (t, log) {
  var lat = ns(time(t))
  var limit = 21e6
  if (lat > limit) {
    log.warn('high latency: ' + (lat / 1e6).toFixed(2) + ' ms')
  }
  return lat
}

function urls (readable, opts, cb) {
  assert(cb, 'callback required')
  var sc = 200
  var t = time()
  var urls = []
  function onend (er) {
    if (!urls) return
    readable.removeListener('data', ondata)
    readable.removeListener('error', onerror)
    readable.removeListener('end', onend)
    var payload = JSON.stringify(urls)
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
  var s = opts.manger.list()
  urls(s, opts, cb)
}

function root (req, res, opts, cb) {
  var payload = JSON.stringify({
    name: 'manger',
    version: opts.version
  })
  cb(null, 200, payload)
}

function ranks (req, res, opts, cb) {
  var s = opts.manger.ranks()
  urls(s, opts, cb)
}

function resetRanks (req, res, opts, cb) {
  cb(null, 202, OK)
  var cache = opts.manger
  cache.resetRanks(function (er) {
    if (er) {
      var error = new Error('could not reset ranks: ' + er.message)
      opts.log.warn(error)
    }
  })
}

function flushCounter (req, res, opts, cb) {
  cb(null, 202, OK)
  var cache = opts.manger
  cache.flushCounter(function (er, count) {
    if (er) opts.log.warn(er)
  })
}

function router () {
  var router = HttpHashRouter()
  router.set('/', root)
  router.set('/entries', {
    POST: entries
  })
  router.set('/entries/:url', {
    GET: entriesOfFeed
  })
  router.set('/feeds', {
    GET: list,
    POST: feeds,
    PUT: update
  })
  router.set('/feed/:url', {
    GET: feed,
    DELETE: deleteFeed
  })
  router.set('/ranks', {
    GET: ranks,
    DELETE: resetRanks,
    PUT: flushCounter
  })
  return router
}

function version () {
  var p = path.join(__dirname, 'package.json')
  var data = fs.readFileSync(p)
  var pkg = JSON.parse(data)
  return pkg.version
}

function defaults (opts) {
  opts = opts || Object.create(null)
  opts.location = opts.location || '/tmp/manger-http'
  opts.port = opts.port || 8384
  opts.log = opts.log || { info: nop, warn: nop, debug: nop, error: nop }
  opts.ttl = opts.ttl || 24 * 3600 * 1000
  opts.cacheSize = opts.cacheSize || 16 * 1024 * 1024
  return opts
}

function MangerService (opts) {
  if (!(this instanceof MangerService)) return new MangerService(opts)

  opts = defaults(opts)
  util._extend(this, opts)

  this.router = router()
  this.version = version()

  this.manger = null
  this.repl = null
  this.server = null

  mkdirp.sync(this.location)
}

// TODO: Name this ReqContext
function ReqOpts (log, manger, updating, version) {
  this.log = log
  this.manger = manger
  this.updating = typeof updating === 'boolean' ? updating : false
  this.version = version
}

function errorHandler (er) {
  if (ok(er)) {
    this.log.warn(er.message)
  } else {
    var failure = 'fatal error'
    var reason = er.message
    var error = new Error([failure, reason].join([': ']))
    this.log.error(error)
    process.nextTick(function () {
      throw error
    })
  }
}

MangerService.prototype.startREPL = function (addr, cb) {
  var me = this
  function listener (sock) {
    var opts = {
      prompt: 'manger> ',
      input: sock,
      output: sock
    }
    var r = repl.start(opts)
    r.context.svc = me
    r.on('exit', function () {
      sock.end()
    })
  }
  var server = net.createServer(listener)
  function onerror (er) {
    if (er.code === 'EADDRINUSE') {
      fs.unlink(addr, function (er) {
        server.listen(addr)
      })
    }
  }
  server.on('error', onerror)
  server.listen(addr, function (er) {
    cb(er, server)
  })
}

MangerService.prototype.start = function (cb) {
  cb = cb || nop

  this.log.info('starting pid %s', process.pid)
  this.log.info('using database at %s', this.location)
  this.log.info('with cache size %s MB', this.cacheSize / 1024 / 1024)

  var cache = manger(this.location, {
    cacheSize: this.cacheSize
  })
  this.errorHandler = errorHandler.bind(this)
  cache.on('error', this.errorHandler)
  this.manger = cache

  var router = this.router
  var ctx = new ReqOpts(this.log, this.manger, false, this.version)

  function onrequest (req, res) {
    ctx.log.info(req.method + ' ' + req.url)

    function terminate (er, statusCode, payload, time) {
      if (er) {
        var payloads = {
          404: function () {
            var reason = req.url + ' is not an endpoint'
            ctx.log.warn(reason)
            statusCode = 404
            payload = JSON.stringify({
              error: 'not found',
              reason: reason
            })
          },
          405: function () {
            var reason = req.method + ' ' + req.url + ' is undefined'
            ctx.log.warn(reason)
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
            ctx.log.error(er)
            throw er
          }
          ctx.log.warn(er)
        } else {
          ctx.log.error(er)
          throw er
        }
      }
      respond(req, res, statusCode, payload, time, ctx)

      req = null
      res = null
    }
    router(req, res, ctx, terminate)
  }
  var me = this

  var server = http.createServer(onrequest)
  server.listen(this.port, function (er) {
    me.log.info('listening on port %s', me.port)
    me.log.info('allowing %s sockets', http.globalAgent.maxSockets)
    cb(er)
  })
  this.server = server

  var addr = '/tmp/manger-repl.sock'
  this.startREPL(addr, function onrepl (er, server) {
    ctx.log.info('REPL started at ' + addr)
    me.repl = server
  })
}

// Only for testing. Restarting is undefined.
MangerService.prototype.stop = function (cb) {
  cb = cb || nop
  var me = this
  function closeServer () {
    var server = me.server
    server ? server.close(next) : next()
  }
  function closeDB () {
    var db = me.manger ? me.manger.db : null
    db ? db.close(next) : next()
  }
  function closeREPL () {
    var repl = me.repl
    repl ? repl.close(next) : next()
  }
  var tasks = [closeServer, closeDB, closeREPL]
  function next (er) {
    if (er) {
      var error = new Error('stop error: ' + er.message)
      me.log.warn(error)
    }
    var f = tasks.shift()
    if (f) {
      f()
    } else {
      if (me.errorHandler) {
        me.manger.removeListener('error', me.errorHandler)
      }
      me.manger = null
      me.repl = null
      me.server = null
      cb()
    }
  }
  next()
}

if (parseInt(process.env.NODE_TEST, 10) === 1) {
  exports.MangerService = MangerService
  exports.debug = debug
  exports.defaults = defaults
  exports.factor = factor
  exports.nop = nop
  exports.ok = ok
  exports.router = router
}
