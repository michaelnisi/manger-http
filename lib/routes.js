'use strict'

// routes - route requests

const assert = require('assert')
const querystring = require('querystring')
const url = require('url')
const { StringDecoder } = require('string_decoder')
const query = require('./query')
const { Query } = require('manger')
const { pipeline, Writable } = require('readable-stream')

module.exports = {
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
}

function feeds (req, res, opts, cb) {
  const s = opts.manger.feeds()

  query(s, req, opts, cb)
}

function urlFromParams (params) {
  const str = params.url

  return typeof str === 'string' ? unescape(str) : null
}

/**
 * Handles requests for single feeds or entries of single feeds. The purpose
 * of these extra routes is to enhance external caching by using these clearcut
 * GET requests for singly things, which are quite common use cases with our
 * clients.
 * 
 * @param {readable} transform The `manger` transform stream.
 * @param {Request} req 
 * @param {Response} res 
 * @param {object} opts 
 * @param {(error: Error, statusCode: number, payload: string, time: number) => {}} cb 
 */
function single (transform, req, _res, opts, cb) {
  const t = process.hrtime()

  const makeError = (error) => {
    return error ? new Error(`${req.url}: ${String(er)}`) : null
  }

  const makePayload = () => {
    return Buffer.from(acc).toString()
  }

  let q
  let acc = []
 
  try {
    const { params } = opts
    const uri = urlFromParams(params)
    q = new Query(uri)
  } catch (error) {
    if (!cb) return
    cb(makeError(error), 200, makePayload(), t)
  }

  pipeline(
    transform,
    new Writable({
      write(chunk, enc, cb) {
        acc.push(chunk)
        cb()
      }
    }),
    error => {
      if (!cb) return
      cb(makeError(error), 200, makePayload(), t)
    }
  )

  transform.end(q)
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
