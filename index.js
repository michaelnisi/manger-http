// manger-http - provide HTTP interface for manger

module.exports = exports = MangerService

var HttpHashRouter = require('http-hash-router')
var Negotiator = require('negotiator')
var StringDecoder = require('string_decoder').StringDecoder
var fs = require('fs')
var http = require('http')
var manger = require('manger')
var mkdirp = require('mkdirp')
var net = require('net')
var path = require('path')
var repl = require('repl')
var util = require('util')
var zlib = require('zlib')

// TODO: Copy all errors

function nop () {}

var debug = (function () {
  return parseInt(process.env.NODE_DEBUG, 10) === 1 ?
    function (o) {
      console.error('** manger-http: %s', util.inspect(o))
    } : nop
})()

function time (t) {
  return t[0] * 1e9 + t[1]
}

function headers (len, lat, enc) {
  var headers = {
    'Cache-Control': 'must-revalidate, max-age=' + 86400,
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

// TODO: Replace with map (er.message in whitelist)
var whitelist = RegExp([
  'client error',
  'invalid query',
  'not deleted',
  'parse error',
  'quaint HTTP status',
  'query error',
  'request error'
].join('|'))

// A general responder for bufferd payloads that applies gzip compression
// if requested.
//
// - req IncommingMessage The request
// - res ServerResponse The response
// - statusCode Number The HTTP status code
// - payload Buffer | String The JSON payload
// - time Array | void The hi-res real time tuple of when the request hit
function respond (req, res, statusCode, payload, time) {
  var log = req.log
  if (res.finished) {
    log.warn('internal error: multiple responses')
    return
  }
  function onfinish () {
    res.removeAllListeners()
  }
  function onclose () {
    log.warn('connection terminated: ' + req.url)
    onfinish()
  }
  var gz = getGz(req)
  function lat () {
    if (time instanceof Array) return latency(time, log)
  }
  if (gz) {
    zlib.gzip(payload, function (er, zipped) {
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
  if (er) {
    var msg = er.message
    if (msg) ok = msg.match(whitelist) !== null
  }
  return ok
}

function query (s, req, opts, cb) {
  var t = process.hrtime()
  var queries = manger.queries()
  var buf = ''
  var onend = done
  function done (er) {
    [s, queries, req].forEach(function (s) {
      s.removeAllListeners()
    })
    var sc = er ? er.statusCode || 404 : 200
    cb(er, sc, buf, t)
    onend = nop
  }
  function onerror (er) {
    var error = new Error(er.message)
    if (!ok(er)) {
      onend(error)
    } else {
      req.log.warn(error)
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
  var s = req.manger.feeds()
  query(s, req, opts, cb)
}

function urlFromParams (params) {
  var str = params.url
  return typeof str === 'string' ? unescape(str) : null
}

function single (s, req, res, opts, cb) {
  var t = process.hrtime()
  var params = opts.params
  var uri = urlFromParams(params)
  var q = manger.query(uri)
  s.end(q)
  var buf = ''
  s.on('readable', function () {
    var chunk
    while ((chunk = s.read()) !== null) {
      buf += chunk
    }
  })
  function onfinish (er) {
    s.removeAllListeners()
    var sc = er ? er.statusCode || 404 : 200
    cb(er, sc, buf, t)
  }
  s.on('finish', onfinish)
  s.on('error', onfinish)
}

function feed (req, res, opts, cb) {
  var s = req.manger.feeds()
  single(s, req, res, opts, cb)
}

function entriesOfFeed (req, res, opts, cb) {
  var s = req.manger.entries()
  single(s, req, res, opts, cb)
}

function deleteFeed (req, res, opts, cb) {
  // TODO: Time only in DEBUG mode
  var t = process.hrtime()
  var params = opts.params
  var uri = urlFromParams(params)
  var cache = req.manger
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

// API: /POST entries
function entries (req, res, opts, cb) {
  var s = req.manger.entries()
  query(s, req, opts, cb)
}

function factor (count) {
  return Math.max(Math.min(Math.round(count * 0.1), 500), 1)
}

function plural (n) {
  return n > 1 || n === 0 ? 's' : ''
}

// API: /GET update
function update (req, res, opts, cb) {
  var log = req.log
  var cache = req.manger
  cache.flushCounter(function (er, feedCount) {
    var s
    function onerror (er) {
      if (s) s.removeAllListeners()
      var payload = JSON.stringify({
        error: 'not updated',
        reason: 'failed to flush counter'
      })
      cb(er, 500, payload)
    }
    if (er) {
      return onerror(er)
    }
    var payload = JSON.stringify({
      ok: true
    })
    cb(null, 202, payload)
    var x = factor(feedCount)
    s = cache.update(x)
    var t = process.hrtime()
    var count = 0
    function ondata (chunk) {
      count++
    }
    function onend () {
      s.removeAllListeners()
      var lat = time(process.hrtime(t))
      var ms = (lat / 1e6).toFixed(2) + ' ms'
      log.info(count + ' feed' + plural(count) +
        ' updated with ' + x + ' stream' +
        plural(x) + ' in ' + ms)
    }
    s.on('data', ondata)
    s.on('error', onerror)
    s.on('end', onend)
  })
}

function latency (t, log) {
  var lat = time(process.hrtime(t))
  if (lat > 40e6) {
    log.warn('high latency: ' + (lat / 1e6).toFixed(2) + ' ms')
  }
  return lat
}

function urls (readable, opts, cb) {
  var sc = 200
  var t = process.hrtime()
  var urls = []
  function done (er) {
    readable.removeAllListeners()
    var payload = JSON.stringify(urls)
    cb(null, sc, payload, t)
    onend = nop
  }
  var onend = done
  function onerror (er) {
    onend(er)
  }
  var decoder
  function decode (chunk) {
    if (!decoder) decoder = new StringDecoder('utf8')
    return decoder.write(chunk)
  }
  function ondata (chunk) {
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
  var s = req.manger.list()
  urls(s, opts, cb)
}

function root (req, res, opts, cb) {
  var payload = JSON.stringify({
    name: 'manger',
    version: req.version
  })
  cb(null, 400, payload)
}

function ranks (req, res, opts, cb) {
  var s = req.manger.ranks()
  urls(s, opts, cb)
}

function resetRanks (req, res, opts, cb) {
  var payload = JSON.stringify({
    ok: true
  })
  cb(null, 202, payload)
  var cache = req.manger
  cache.resetRanks(function (er) {
    if (er) {
      var error = new Error('could not reset ranks: ' + er.message)
      req.log.warn(error)
    }
  })
}

function flushCounter (req, res, opts, cb) {
  var payload = JSON.stringify({
    ok: true
  })
  cb(null, 202, payload)
  var cache = req.manger
  cache.flushCounter(function (er, count) {
    if (er) req.log.warn(er)
  })
}

function defaults (opts) {
  opts = opts || Object.create(null)
  opts.location = opts.location || '/tmp/manger-http'
  opts.port = opts.port || 8384
  opts.log = opts.log || { info: nop, warn: nop, debug: nop, error: nop }
  opts.ttl = opts.ttl || 24 * 36e5
  opts.cacheSize = opts.cacheSize || 16 * 1024 * 1024
  return opts
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

function MangerService (opts) {
  opts = defaults(opts)
  if (!(this instanceof MangerService)) return new MangerService(opts)
  util._extend(this, opts)
  mkdirp.sync(this.location)
  this.version = version()
  this.router = router()
}

MangerService.prototype.handle = function (req, res) {
  this.log.info('/' + req.method + ' ' + req.url)
  req.log = this.log
  req.version = this.version
  req.manger = this.manger
  req.handle = this.handle

  function terminate (er, statusCode, payload, time) {
    if (er) {
      if (er.type === 'http-hash-router.not-found') {
        var reason = req.url + ' is not an endpoint'
        req.log.warn(reason)
        statusCode = 404
        payload = JSON.stringify({
          error: 'not found',
          reason: reason
        })
      } else if (er.message.match(whitelist)) {
        req.log.warn(er.message)
      } else {
        req.log.error(er)
        throw er
      }
    }
    respond(req, res, statusCode, payload, time)
  }
  this.router(req, res, {}, terminate)
}

// TODO: Review starting and stopping
MangerService.prototype.start = function (cb) {
  this.log.info('starting pid %s on port %s', process.pid, this.port)
  this.log.info('using database at %s', this.location)
  this.log.info('with cache size %s MB', this.cacheSize / 1024 / 1024)
  this.log.info('maximal %s sockets', http.globalAgent.maxSockets)

  this.manger = this.manger || manger(this.location, {
    cacheSize: this.cacheSize
  })
  var me = this
  this.server = this.server || http.createServer(function (req, res) {
    me.handle(req, res)
  })
  this.server.once('listening', function () {
    if (cb) cb()
  })
  this.server.listen(this.port)
}

// TODO: Integrate REPL
MangerService.prototype.startREPL = function () {
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
  var name = '/tmp/manger-repl.sock'
  var server = net.createServer(listener)
  function onerror (er) {
    if (er.code === 'EADDRINUSE') {
      fs.unlink(name, function (er) {
        server.listen(name)
      })
    }
  }
  server.on('error', onerror)
  server.listen(name)
}

MangerService.prototype.stop = function (cb) {
  cb = cb || nop
  var db = this.db
  this.server.close(function (er) {
    if (db) {
      db.close(function (er) {
        cb(er)
      })
    } else {
      cb(er)
    }
  })
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
