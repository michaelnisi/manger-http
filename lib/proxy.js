'use strict'

// proxy - access reverse proxy cache

const h = process.env.REVERSE_PROXY_HOST
const p = Number(process.env.REVERSE_PROXY_PORT)

const http = require('http')
const https = require('https')
const { createLogger } = require('./meta')

// Exports are NOP if the environment isn’t set up.
if (h && p) {
  exports.purge = purge
} else {
  createLogger().info('ignoring proxy: environment not set up')
  exports.purge = () => {}
}

// Optional API key.
const k = process.env.REVERSE_PROXY_API_KEY

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
  port = p
) {
  this.route = route
  this.url = url
  this.headers = headers
  this.hostname = hostname
  this.method = method
  this.port = port
}

function issue (p = new ProxyRequest(), secret, log = createLogger()) {
  log.info('purging: ( %s, %s )', p.route, p.url)

  const h = p.headers

  if (typeof secret === 'string') {
    h['Authentication'] = `Basic ${secret}`
  }

  const opts = {
    hostname: p.hostname,
    port: p.port,
    path: `${p.route}/${encodeURIComponent(p.url)}`,
    headers: h,
    method: p.method
  }

  log.debug('making request: %o', opts)

  const mod = opts.port === 443 ? https : http

  const req = mod.request(opts, (res) => {
    const sc = res.statusCode

    if (sc === 200) {
      log.info('purged: %s', p.url)
    } else {
      log.warn('purging failed: %i', sc)
    }

    res.resume()
  })

  req.on('error', (er) => {
    log.error('purge error: %s', er)
  })

  req.end()
}

// Purges feed and entries at url, passing secret for basic access
// authentication, while printing with log. Soft purging is what
// it’s called.
function purge (url, secret, log) {
  issue(new ProxyRequest('/feed', url), log)
  issue(new ProxyRequest('/entries', url), log)
}
