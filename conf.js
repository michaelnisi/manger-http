'use strict'

// config - configure manger-http

const bunyan = require('bunyan')
const http = require('http')

// Working around https://github.com/nodejs/node/issues/16196
require('tls').DEFAULT_ECDH_CURVE = 'auto'

function level (l) {
  return [10, 20, 30, 40, 50, 60].includes(l) ? l : null
}

function log () {
  const l = level(parseInt(process.env.MANGER_LOG_LEVEL, 10))
  if (!l) return null
  return bunyan.createLogger({
    name: 'manger',
    level: l,
    serializers: bunyan.stdSerializers
  })
}

const MAX_UPDATES = 8.64e7

function maxUpdates () {
  const max = parseInt(process.env.MANGER_MAX_UPDATES, 10)
  return isNaN(max) ? MAX_UPDATES : max
}

exports.cacheSize = process.env.LEVEL_DB_CACHE_SIZE
exports.location = process.env.LEVEL_DB_LOCATION
exports.log = log()
exports.maxSockets = http.globalAgent.maxSockets = 4096
exports.maxUpdates = maxUpdates()
exports.port = process.env.PORT

if (module === require.main) {
  console.log(exports)
}
