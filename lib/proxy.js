'use strict'

// proxy - access reverse proxy cache

const http = require('http')
const https = require('https')
const { createLogger } = require('./meta')

// Accessing process environment for settings.

const h = process.env.REVERSE_PROXY_HOST
const p = Number(process.env.REVERSE_PROXY_PORT)

// Optional API key.
const k = process.env.REVERSE_PROXY_API_KEY

// Exports are NOP if environment isn’t set up.
if (h && p) {
  exports.purge = purge
} else {
  createLogger().warn('ignoring proxy: environment not set up')
  exports.purge = () => {}
}

// Returns new HTTP headers for issueing a proxy request.
function createHeaders () {
  let h = {
    'Fastly-Soft-Purge': 1,
    'accept': 'application/json'
  }

  // Currently not in use.

  if (k) {
    h['fastly-key'] = k
  }

  return h
}

// A request with Fastly Soft Purge defaults.
// https://docs.fastly.com/guides/purging/soft-purges
function ProxyRequest (
  route = '/feed',
  url = 'https://daringfireball.net/thetalkshow/rss',
  headers = createHeaders(),
  hostname = h,
  method = 'PURGE',
  port = p,
) {
  this.route = route
  this.url = url
  this.headers = headers
  this.hostname = hostname
  this.method = method
  this.port = port
}

// Returns encoded URI appending url to route.
ProxyRequest.prototype.getPath = function () {
  return `${this.route}/${encodeURIComponent(this.url)}`
}

// Timestamp of the last request error or failure.
let failedAt = 0

// Issues request including secret in headers.
function issue (p = new ProxyRequest(), secret, log = createLogger()) {
  log.info('issueing: ( %s, %s )', p.route, p.url)

  const h = p.headers

  if (typeof secret === 'string') {
    h['Authorization'] = `Basic ${secret}`
  }

  const path = p.getPath()

  if (typeof path !== 'string') {
    return log.warn('aborting: invalid route or url')
  }

  // These requests come in cascades, we are trying to be defensive. This whole
  // proxy thing is optional and thus must not affect general operation.
  if ((new Date().getTime() - failedAt) < 300000) {
    return log.warn('aborting: previous failure within five minutes')
  }

  const opts = {
    hostname: p.hostname,
    port: p.port,
    path: p.getPath(),
    headers: h,
    method: p.method
  }

  log.debug('making request: %o', opts)

  const mod = opts.port === 443 ? https : http

  const req = mod.request(opts, (res) => {
    const sc = res.statusCode

    if (sc === 200) {
      log.info('success: %s', p.url)
    } else {
      log.warn('failure: %i', sc)
      failedAt = new Date().getTime()
    }

    // Being fire-and-forget, we are just fully consuming the body.
    res.resume()
  })

  req.on('error', (er) => {
    log.error('purge error: %s', er)
    failedAt = new Date().getTime()
  })

  req.end()
}

// Purges feed and entries at url, passing secret for basic access
// authentication, while printing with log. Soft purging is what
// it’s called. This is a fire-and-forget operation without callback.
function purge (url, secret, log) {
  issue(new ProxyRequest('/feed', url), secret, log)
  issue(new ProxyRequest('/entries', url), secret, log)
}
