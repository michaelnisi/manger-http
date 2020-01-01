// meta - details for operating this service
// @ts-check

const {debuglog} = require('util');
const assert = require('assert');

const debug = debuglog('manger-http');

function createLogger(log) {
  return (
    log || {
      fatal: debug,
      error: debug,
      warn: debug,
      info: debug,
      debug,
      trace: debug,
    }
  );
}

/**
 * A custom error class for this service.
 */
class MangerServiceError extends Error {
  /**
   * Creates a new error.
   *
   * @param {string} message
   * @param {number} statusCode
   */
  constructor(message, statusCode) {
    super(message);

    this.statusCode = statusCode;

    Error.captureStackTrace(this, this.constructor);
  }
}

/**
 * Whitelists errors we tolerate without crashing.
 */
const whitelist = new RegExp(
  [
    'ECONNREFUSED',
    'ECONNRESET',
    'ENOTFOUND',
    'ETIMEDOUT',
    'certificate',
    'client error',
    'currently updating',
    'not deleted',
    'parse error',
    'quaint HTTP status',
    'query error',
    'request error',
    'socket hang up',
    'socket timeout',
    'too many redirects',
    'update error: write EPIPE',
  ].join('|'),
  'i',
);

/**
 * Returns a new service error from `error`.
 *
 * @param {*} error
 */
function createServiceError(error) {
  if (!error) {
    return null;
  }

  if (error instanceof MangerServiceError) {
    return error;
  }

  assert(error instanceof Error);

  const {statusCode, message} = error;

  if (statusCode === 405) {
    return new MangerServiceError('method not allowed', statusCode);
  }

  if (!message) {
    return new MangerServiceError('unexpected fatal', 500);
  }

  if (message.match(whitelist) !== null) {
    return new MangerServiceError(message, 200);
  }

  return new MangerServiceError(message, 500);
}

/**
 *
 * @param {MangerServiceError} er
 * @param {*} log
 */
function crash(er, log) {
  if (log && typeof log.fatal === 'function') {
    log.fatal(er);
  }
  process.nextTick(() => {
    throw er;
  });
}

/**
 * Returns summed nanoseconds from a [seconds, nanoseconds] tuple Array.
 *
 * @param {[number, number]} t
 */
function nanoseconds(t) {
  return t[0] * 1e9 + t[1];
}

/**
 * Returns latency in milliseconds and logs if it exceeds 20 ms. Having no
 * control over how long outbound HTTP requests for fetching unknown feeds
 * take, this isn’t a rare occurrence. Trying to differentiate using log
 * levels, warning above one and a half seconds. The goal is to prevent
 * flooding our production logs, which are at level 40 (warn).
 *
 * @param {[number, number]} t
 * @param {*} log
 */
function latency(t, log) {
  if (!Array.isArray(t)) {
    return -1;
  }

  const lat = nanoseconds(process.hrtime(t));

  if (lat > 2e7) {
    const ms = lat / 1e6;

    if (ms > 1500 && log && typeof log.warn === 'function') {
      log.warn({ms: ms.toFixed(2)}, 'latency');
    }
  }

  return lat;
}

module.exports = {
  crash,
  createLogger,
  latency,
  createServiceError,
  MangerServiceError,
};
