// config - configure manger-http

const pino = require('pino');

// Working around https://github.com/nodejs/node/issues/16196
require('tls').DEFAULT_ECDH_CURVE = 'auto';

const {
  MANGER_MAX_UPDATES,
  MANGER_LOG_LEVEL,
  LEVEL_DB_CACHE_SIZE,
  LEVEL_DB_LOCATION,
  PORT,
} = process.env;

const MAX_UPDATES = 8.64e7;

function maxUpdates() {
  const max = parseInt(MANGER_MAX_UPDATES, 10);

  return isNaN(max) ? MAX_UPDATES : max;
}

function createLogger() {
  const level = MANGER_LOG_LEVEL;

  if (!level) {
    return null;
  }

  const dest = pino.extreme();

  return pino({level, dest});
}

exports.cacheSize = Number(LEVEL_DB_CACHE_SIZE);
exports.location = LEVEL_DB_LOCATION;
exports.log = createLogger();
exports.maxUpdates = maxUpdates();
exports.port = parseInt(PORT, 10);

if (module === require.main) {
  // eslint-disable-next-line no-console
  console.log(exports);
}
