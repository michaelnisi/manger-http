// common - common test stuff

const server = require('../..')

exports.freshName = freshName
exports.freshMangerServer = freshMangerServer

function freshName () {
  return `/tmp/manger-http-${Math.floor(Math.random() * (1 << 24))}`
}

function freshMangerServer () {
  const opts = {
    location: this.freshName(),
    port: 1337
  }
  return server(opts)
}
