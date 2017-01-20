'use strict'

// config - configure manger-http

const bunyan = require('bunyan')
const http = require('http')

function level (l) {
  return [10, 20, 30, 40, 50, 60].includes(l) ? l : null
}

function log () {
  const l = level(parseInt(process.env.NODE_LOG_LEVEL, 10))
  if (!l) return null
  return bunyan.createLogger({
    name: 'manger',
    level: l,
    serializers: bunyan.stdSerializers
  })
}

exports.log = log()
exports.location = process.env.LEVEL_DB_LOCATION
exports.cacheSize = process.env.LEVEL_DB_CACHE_SIZE
exports.port = process.env.PORT
exports.maxSockets = http.globalAgent.maxSockets = 4096

if (module === require.main) {
  console.log(exports)
  process.exit(0)
}
