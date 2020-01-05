// update - update all
// @ts-check

module.exports = exports = update;

const {crash, latency, createServiceError} = require('./meta');

const OK = JSON.stringify({ok: true});
const NOT_OK = JSON.stringify({ok: false});

/**
 * Updates the cache if `this` is a `MangerService` object.
 *
 * @param {*} _req The HTTP request.
 * @param {*} _res The HTTP response.
 * @param {{maxUpdates,manger,log}} opts Time limit between updates.
 * @param {function} cb A callback receiving error, status code, and payload.
 */
function update(_req, _res, opts, cb = () => {}) {
  const {maxUpdates} = opts;
  const now = Date.now();
  const then = this.updating;

  const locked = typeof then === 'number' ? now - then < maxUpdates : false;

  if (locked) {
    cb(new Error('locked: currently updating'), 423, NOT_OK);

    return;
  }

  // We are using a timestamp, instead of a plain boolean, for our updating flag
  // limiting the maximum duration, currently one day, of the lock.
  this.updating = now;

  const {manger, log} = opts;

  manger.flushCounter((counterError, feedCount) => {
    if (counterError) {
      const failure = 'not updated';
      const reason = counterError.message;
      const error = new Error(`${failure}: ${reason}`);
      const payload = JSON.stringify({error: failure, reason});

      this.updating = null;

      cb(error, 500, payload);

      return;
    }

    cb(null, 202, OK);

    if (feedCount === 0) {
      log.warn('empty cache');

      this.updating = null;

      return;
    }

    const t = process.hrtime();

    manger.update((updateError, urls) => {
      this.updating = null;

      if (updateError) {
        const er = createServiceError(updateError);

        if (er.statusCode !== 500) {
          log.warn(er, 'update');
        } else {
          crash(er, log);
        }

        return;
      }

      if (urls.length > 0) {
        log.warn(
          {feeds: urls.length, ms: latency(t, log).toFixed(2)},
          'updated',
        );
      } else {
        log.warn('no updates');
      }
    });
  });
}
