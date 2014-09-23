
// manger-http - Manger HTTP API

module.exports = exports = MangerService

var assert = require('assert')
  , http = require('http')
  , levelup = require('levelup')
  , manger = require('manger')
  , mkdirp = require('mkdirp')
  , routes = require('routes')
  , util = require('util')
  , querystring = require('querystring')
  ;

function noop () {}

var debug = function () {
  return process.env.NODE_DEBUG ?
    function (o) {
      console.error('**manger-http: %s', util.inspect(o))
    } : noop
}()

// Whitelist error messages that should be warnings.
var _warn = [
]

function warn (er) {
  return er.notFound || _warn.indexOf(er.message) > -1
}

function streamError (log, stream, req, res) {
  return function (er) {
    warn(er) ? log.warn(er.message) : log.error({req:req, err:er})
    stream.unpipe(res)
    stream.removeAllListeners()
    res.end('[]\n')
  }
}

function ping (req, res, params) {
  req.log.info('ping')
  res.end('pong\n')
}

function notfound (req, res) {
  req.log.warn('fishy request')
  res.writeHead(404)
  res.end('not found\n')
}

function list (req, res) {
  req.log.info('list')
  var stream = req.manger.list()
  stream.pipe(res)
  stream.once('error', streamError(req.log, stream, req, res))
}

function feeds (req, res) {
  req.log.info('feeds')
  var queries = new manger.Queries()
  var feeds = req.manger.feeds()
  feeds.once('error', streamError(req.log, feeds, req, res))
  queries.once('error', streamError(req.log, queries, req, res))
  req.pipe(queries).pipe(feeds).pipe(res)
}

function entries (req, res) {
  req.log.info('entries')
  var queries = new manger.Queries()
  var entries = req.manger.entries()
  entries.once('error', streamError(req.log, entries, req, res))
  queries.once('error', streamError(req.log, queries, req, res))
  req.pipe(queries).pipe(entries).pipe(res)
}

function update (req, res) {
  var log = req.log
  log.info('update')
  var update = req.manger.update()
  update.on('error', function (er) {
    if (er.message.match(/connect/) || er.message.match(/no etag/)) {
      log.warn(er.message)
    } else {
      log.error({req:req, err:er})
      res.end(JSON.stringify(er))
    }
  })
  update.pipe(res)
}

function defaults (opts) {
  opts = opts || Object.create(null)
  opts.location = opts.location || '/tmp/manger-http'
  opts.port = opts.port || 8384
  opts.log = opts.log || { info:noop, warn:noop, debug:noop, error:noop }
  var hour = 3600
  opts.ttl = opts.ttl || 1 * hour
  return opts
}

function MangerService (opts) {
  opts = defaults(opts)
  if (!(this instanceof MangerService)) return new MangerService(opts)
  util._extend(this, opts)

  var router = this.router = routes()
  router.addRoute('/entries', entries)
  router.addRoute('/feeds', feeds)
  router.addRoute('/list', list)
  router.addRoute('/ping', ping)
  router.addRoute('/update', update)
  router.addRoute('/*', notfound)
  router.addRoute('/', notfound)

  mkdirp.sync(this.location)
}

MangerService.prototype.route = function (req) {
  return this.router.match(req.url)
}

MangerService.prototype.handle = function (req, res) {
  var rt = this.route(req, res)
  res.setHeader('Cache-Control', 'max-age=' + this.ttl)
  res.setHeader('Content-Type', 'application/json')
  if (req.method === 'HEAD') {
    return res.end()
  }
  var me = this
  res.on('close', function () {
    me.log.warn({err:new Error('connection terminated'), req:req})
  })

  req.log = this.log
  req.manger = this.manger
  req.handle = this.handle

  rt.fn(req, res, rt.params)
}

MangerService.prototype.start = function (cb) {
  this.log.info('starting on port %s', this.port)
  this.log.info('using database at %s', this.location)
  this.db = this.db || levelup(this.location)
  if (!this.db.isClosed) this.db.open()
  this.manger = this.manger || manger({ db:this.db }) 
  var me = this
  this.server = this.server || http.createServer(function (req, res) {
    me.handle(req, res)
  })
  if (cb) this.server.once('listening', cb)
  this.server.listen(this.port)
}

MangerService.prototype.stop = function (cb) {
  var me = this
  this.server.close(function (er) {
    me.db.close(function (er) {
      if (cb) cb(er)
    })
  })
}

if (process.env.NODE_TEST) {
  exports.MangerService = MangerService
  exports.defaults = defaults
  exports.noop = noop
}
