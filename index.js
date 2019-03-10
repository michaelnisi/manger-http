'use strict'

// manger-http - serve podcast feeds with persistent caching

module.exports = exports = MangerService

const HttpHash = require('http-hash')
const fs = require('fs')
const http = require('http')
const httpMethods = require('http-methods/method')
const manger = require('manger')
const mkdirp = require('mkdirp')
const path = require('path')
const update = require('./lib/update')
const url = require('url')
const { crash, ok, createLogger } = require('./lib/meta')
const { respond } = require('./lib/respond')

const {
  deleteFeed,
  entries,
  entriesOfFeed,
  feed,
  feeds,
  flushCounter,
  list,
  ranks,
  resetRanks,
  root
} = require('./lib/routes')

// Returns the version of the containing package.
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
    // HAProxy default health checking connects and disconnects every two
    // seconds, producing a heartbeat here, hence the discreet logging.
    socket.once('close', () => { log.trace(er.message) })
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
  exports.ok = ok
}
