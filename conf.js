// config - configure manger-http

const pino = require('pino');

// Working around https://github.com/nodejs/node/issues/16196
require('tls').DEFAULT_ECDH_CURVE = 'auto';

const MAX_UPDATES = 8.64e7;

function maxUpdates() {
  const max = parseInt(process.env.MANGER_MAX_UPDATES, 10);

  return isNaN(max) ? MAX_UPDATES : max;
}

function createLogger() {
  return pino({level: process.env.MANGER_LOG_LEVEL || 'info'});
}

exports.cacheSize = Number(process.env.LEVEL_DB_CACHE_SIZE);
exports.location = process.env.LEVEL_DB_LOCATION;
exports.log = createLogger();
exports.maxUpdates = maxUpdates();
exports.port = parseInt(process.env.PORT, 10);

if (module === require.main) {
  // eslint-disable-next-line no-console
  console.log(exports);
}
