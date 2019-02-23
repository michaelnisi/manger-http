// proxy - access reverse proxy cache

exports = module.exports

exports.purge = purge
exports.ProxyContent = Content

const http = require('http')
const https = require('https')
const { createLogger } = require('./log')

function Content (
  route = '/feed',
  url = 'https://daringfireball.net/thetalkshow/rss',
  headers = { 'Fastly-Soft-Purge': 1 },
  hostname = 'localhost',
  method = 'PURGE',
  port = 8384
) {
  this.route = route
  this.url = url
  this.headers = headers
  this.hostname = hostname
  this.method = method
  this.port = port
}

// By default this issues a Fastly Soft Purge marking content as stale.
function purge (p = new Content(), log = createLogger()) {
  log.info('purging: %s', p.url)

  const opts = {
    hostname: p.hostname,
    port: p.port,
    path: `${p.route}/${encodeURIComponent(p.url)}`,
    headers: p.headers,
    method: p.method
  }

  log.debug('making request: %o', opts)

  const testrun = p.hostname === 'localhost'

  const req = (testrun ? http : https).request(opts, (res) => {
    const sc = res.statusCode

    if (sc === 200) {
      log.info('received response: %i', sc)
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
