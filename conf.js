'use strict'

// config - configure manger-http

var bunyan = require('bunyan')
var http = require('http')

function level (l) {
  return [10, 20, 30, 40, 50, 60].includes(l) ? l : bunyan.WARN
}

function log () {
  return bunyan.createLogger({
    name: 'manger',
    level: level(parseInt(process.env.NODE_LOG_LEVEL, 10)),
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
