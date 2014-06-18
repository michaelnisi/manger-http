
// config - configure troubled-www

var path = require('path')
  , fs = require('fs')
  , assert = require('assert')
  ;

exports.port = process.env.NODE_ENV === 'production' ? 80 : 8080
exports.db = process.env.MANGER_DB || '/tmp/mangerdb'

if (module === require.main) {
  console.log(exports)
  process.exit(0)
}
