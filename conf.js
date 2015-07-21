// config - configure manger-http

var bunyan = require('bunyan')
var http = require('http')

function log () {
  if (parseInt(process.env.NODE_SILENT, 10) === 1) return
  var isDebug = parseInt(process.env.NODE_DEBUG, 10) === 1
  return bunyan.createLogger({
    name: 'manger',
    level: isDebug ? 'debug' : 'warn',
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
