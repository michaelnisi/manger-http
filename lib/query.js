// query - query manger
// @ts-check

const {Writable, pipeline} = require('readable-stream');
const {Queries} = require('manger');

/**
 * Queries the Manger cache piping `request` via Queries into `transform`
 * and applies `cb` with an error and the accumulated result.
 *
 * @param {*} transform
 * @param {*} request
 * @param {*} opts
 * @param {function} queryComplete
 */
function query(transform, request, opts, queryComplete = () => {}) {
  const time = process.hrtime();
  const queries = new Queries();

  const acc = [];

  pipeline(
    request,
    queries,
    transform,
    new Writable({
      write(chunk, _enc, cb) {
        acc.push(chunk);
        cb();
      },
    }),
    error => {
      const statusCode = error ? error.statusCode || 404 : 200;
      const body = Buffer.concat(acc).toString();

      queryComplete(error, statusCode, body, time);
    },
  );
}

module.exports = query;
