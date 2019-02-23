'use strict'

// proxy - access reverse proxy cache

exports = module.exports

// Exports are NOP if the environment isn’t set up.
if (process.env.REVERSE_PROXY_HOST && process.env.REVERSE_PROXY_PORT) {
  exports.purge = purge
} else {
  exports.purge = () => {}
}

const http = require('http')
const https = require('https')
const { createLogger } = require('./log')

// A request with Fastly Soft Purge defaults.
// https://docs.fastly.com/guides/purging/soft-purges
function ProxyRequest (
  route = '/feed',
  url = 'https://daringfireball.net/thetalkshow/rss',
  headers = { 'Fastly-Soft-Purge': 1 },
  hostname = process.env.REVERSE_PROXY_HOST,
  method = 'PURGE',
  port = process.env.REVERSE_PROXY_PORT
) {
  this.route = route
  this.url = url
  this.headers = headers
  this.hostname = hostname
  this.method = method
  this.port = port
}

// Issues a request on the reverse proxy and logs the result.
function issue (p = new ProxyRequest(), log = createLogger()) {
  log.info('purging: ( %s, %s )', p.route, p.url)

  const opts = {
    hostname: p.hostname,
    port: p.port,
    path: `${p.route}/${encodeURIComponent(p.url)}`,
    headers: p.headers,
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

function purge (url, log) {
  issue(new ProxyRequest('/feed', url), log)
  issue(new ProxyRequest('/entries', url), log)
}
