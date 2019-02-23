// log - log even if you don’t

exports = module.exports
exports.createLogger = createLogger

const { debuglog } = require('util')

const debug = debuglog('manger-http')

function createLogger (l) {
  return l || {
    fatal: debug,
    error: debug,
    warn: debug,
    info: debug,
    debug: debug,
    trace: debug
  }
}
