// common - common test stuff

exports.freshServer = freshServer
exports.freshName = freshName

var server = require('../..')

function freshName () {
  var name = '/tmp/manger-http-'
  return name + Math.floor(Math.random() * (1 << 24))
}

function freshServer () {
  var opts = {
    location: freshName(),
    port: 1337
  }
  return server(opts)
}
