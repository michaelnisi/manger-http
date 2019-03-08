'use strict'

const assert = require('assert')
const manger = require('manger')
const querystring = require('querystring')
const url = require('url')
const { StringDecoder } = require('string_decoder')

exports.deleteFeed = deleteFeed
exports.entries = entries
exports.entriesOfFeed = entriesOfFeed
exports.feed = feed
exports.feeds = feeds
exports.flushCounter = flushCounter
exports.list = list
exports.ranks = ranks
exports.resetRanks = resetRanks
exports.root = root

// Pipes request, via Queries, into Transform s and applies cb with result.
function query (s, req, opts, cb = () => {}) {
  const t = process.hrtime()
  const queries = manger.Queries()

  // Result

  let err = null
  let buf = []

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

  const onQueriesData = (q) => {
    opts.log.debug(q, 'query')
  }

  // Transform

  const onStreamError = (er) => {
    err = new Error(er.message)
  }

  const onStreamData = (chunk) => {
    buf.push(chunk)
  }

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
    const body = Buffer.concat(buf).toString()

    cb(err, sc, body, t)
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

const OK = JSON.stringify({ ok: true })

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
