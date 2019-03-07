'use strict'

// meta - run this thing

exports.crash = crash
exports.createLogger = createLogger
exports.latency = latency
exports.ok = ok

const { debuglog } = require('util')

const debug = debuglog('manger-http')

function createLogger (log) {
  return log || {
    fatal: debug,
    error: debug,
    warn: debug,
    info: debug,
    debug: debug,
    trace: debug
  }
}

// Whitelists errors we tolerate without crashing.
const whitelist = new RegExp([
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
  'socket timeout',
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

function crash (er, log) {
  if (log && typeof log.fatal === 'function') log.fatal(er)
  process.nextTick(() => { throw er })
}

// Returns summed nanoseconds from a [seconds, nanoseconds] tuple Array.
function nanoseconds (t) {
  return t[0] * 1e9 + t[1]
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

    if (ms > 1500 && log && typeof log.warn === 'function') {
      log.warn({ ms: ms }, 'latency')
    }

    return ms
  }
}
