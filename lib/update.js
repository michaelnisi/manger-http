// update - update all
// @ts-check

module.exports = exports = update;

const {crash, latency, ok} = require('./meta');

const OK = JSON.stringify({ok: true});
const NOT_OK = JSON.stringify({ok: false});

/**
 * Updates the cache if `this` is a `MangerService` object.
 *
 * @param {*} req The HTTP request.
 * @param {*} res The HTTP response.
 * @param {{maxUpdates}} opts Time limit between updates.
 * @param {function} cb A callback receiving error, status code, and payload.
 */
function update(req, res, opts, cb = () => {}) {
  const {maxUpdates} = opts;
  const now = Date.now();
  const then = this.updating;

  const locked = typeof then === 'number' ? now - then < maxUpdates : false;

  if (locked) {
    return cb(new Error('locked: currently updating'), 423, NOT_OK);
  }

  // We are using a timestamp, instead of a plain boolean, for our updating flag
  // limiting the maximum duration, currently one day, of the lock.
  this.updating = now;

  const cache = opts.manger;
  const log = opts.log;

  cache.flushCounter((er, feedCount) => {
    if (er) {
      const failure = 'not updated';
      const reason = er.message;
      const error = new Error(`${failure}: ${reason}`);
      const payload = JSON.stringify({error: failure, reason});

      this.updating = null;

      return cb ? cb(error, 500, payload) : null;
    }

    if (cb) { cb(null, 202, OK); }

    if (feedCount === 0) {
      log.warn('empty cache');
      return (this.updating = null);
    }

    const t = process.hrtime();

    cache.update((error, urls) => {
      if (error) {
        if (ok(error)) {
          log.warn(error, 'update');
        } else {
          crash(error, opts.log);
        }

        return;
      }

      if (urls.length > 0) {
        opts.log.warn({feeds: urls.length, ms: latency(t)}, 'updated');
      } else {
        opts.log.warn('no updates');
      }

      this.updating = null;
    });
  });
}
