'use strict'

// update - update all

module.exports = exports = update

const assert = require('assert')
const { crash, latency, ok } = require('./meta')
const { Manger } = require('manger')

const OK = JSON.stringify({ ok: true })
const NOT_OK = JSON.stringify({ ok: false })

/**
 * Updates the cache, where `this` is a `MangerService` object.
 *
 * @param {*} req
 * @param {*} res
 * @param {*} opts
 * @param {*} cb
 */
function update (req, res, opts, cb) {
  assert(this instanceof Manger)

  const now = Date.now()
  const then = this.updating
  const limit = opts.maxUpdates

  const locked = typeof then === 'number' ? now - then < limit : false

  if (locked) {
    const er = new Error('locked: currently updating')

    return cb(er, 423, NOT_OK)
  }

  // We are using a timestamp, instead of a plain boolean, for our updating flag to
  // limit the duration, currently one day, of the lock.
  this.updating = now

  const cache = opts.manger
  const log = opts.log

  cache.flushCounter((er, feedCount) => {
    if (er) {
      const failure = 'not updated'
      const reason = er.message
      const error = new Error(`${failure}: ${reason}`)
      const payload = JSON.stringify({ error: failure, reason: reason })

      this.updating = null

      return cb ? cb(error, 500, payload) : null
    }

    if (cb) cb(null, 202, OK)

    if (feedCount === 0) {
      log.warn('empty cache')
      return (this.updating = null)
    }

    // Let’s get to work.

    const t = process.hrtime()

    const feedsPerStream = 64

    // Investigating a CPU saturation issue on Fargate, limited to two streams.
    // We are observing steep spikes of CPU utilization while updating.

    const x = Math.min(Math.ceil(feedCount / feedsPerStream), 2)
    const s = cache.update(x)

    let count = 0

    const ondata = (feed) => {
      log.debug('updated: %s', feed.url)

      count++
    }

    const errors = []

    const onerror = (er) => {
      if (ok(er)) {
        log.warn({ err: er, url: er.url }, 'update')
        errors.push(er)
      } else {
        const failure = 'update error'
        const reason = er.message
        const error = new Error(failure + ': ' + reason)

        crash(error, opts.log)
      }
    }

    const onend = () => {
      s.removeListener('data', ondata)
      s.removeListener('error', onerror)
      s.removeListener('end', onend)

      if (count > 0) {
        const info = { feeds: count, streams: x, ms: latency(t) }
        opts.log.warn(info, 'updated')
      } else if (feedCount > 5 && errors.filter((er) => {
        // If we have received 'ENOTFOUND' for all feeds, we've been trying to
        // update, tolerating maximally five of five, we can assume that we
        // have no outbound Internet connectivity -- although we've just been
        // counting errors, without relating them to the feeds. To get the
        // attention of an operator we deliberately opt to crash.

        return er.code === 'ENOTFOUND'
      }).length >= feedCount) {
        const er = new Error('no connection')

        return crash(er, opts.log)
      } else {
        opts.log.warn('no updates')
      }

      this.updating = null
    }

    s.on('data', ondata)
    s.on('error', onerror)
    s.on('end', onend)
  })
}
