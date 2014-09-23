
// config - configure manger-http

var bunyan = require('bunyan')

function log() {
  return bunyan.createLogger({
    name: 'manger-http'
  , level: 'info'
  , serializers: bunyan.stdSerializers
  })
}

exports.log = log()
exports.location = process.env.LEVEL_DB_LOCATION
exports.port = process.env.PORT

if (module === require.main) {
  console.log(exports)
  process.exit(0)
}
