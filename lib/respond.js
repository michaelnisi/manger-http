'use strict'

const Negotiator = require('negotiator')
const assert = require('assert')
const etag = require('etag')
const zlib = require('zlib')
const { latency } = require('./meta')

exports.getGz = getGz
exports.headers = headers
exports.isMatching = isMatching
exports.respond = respond

// Returns true if headers contain matching tag.
function isMatching (headers, tag) {
  let tags = headers['If-None-Match'] || headers['if-none-match']

  if (typeof tags !== 'string') return false

  if (tags.startsWith('W/')) {
    tags = tags.substr(2)
  }

  const tokens = tags.split(',')

  const trimmed = tokens.map(t => {
    return t.trim().split('"').join('')
  })

  const t = tag.split('"').join('')

  return trimmed.indexOf(t) !== -1 || trimmed.indexOf('*') !== -1
}

// Returns true if the request prefers gzip encoding.
function getGz (req) {
  const neg = new Negotiator(req)

  return neg.preferredEncoding(['gzip', 'identity']) === 'gzip'
}

// Returns HTTP headers for content-length, latency, encoding, Etag, and max-age.
function headers (len, lat, enc, tag, ttl = 86400) {
  const sx = 1

  const headers = {
    'Cache-Control': `max-age=${ttl}`,
    'Content-Length': len,
    'Content-Type': 'application/json; charset=utf-8',
    'Surrogate-Control': `max-age=${ttl * sx}`
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

      // Demanding a max-age of one hour for mutator methods.
      const ttl = req.method !== 'GET' ? 3600 : undefined

      const h = headers(zipped.length, latency(time, log), 'gzip', tag, ttl)

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
