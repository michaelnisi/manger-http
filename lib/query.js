// query - query manger
// @ts-check

const {Writable, pipeline} = require('readable-stream');
const {Queries} = require('manger');
const {MangerServiceError} = require('./meta');

/**
 * Queries the Manger cache piping `request` via Queries into `transform`
 * and applies `cb` with an error and the accumulated result.
 *
 * @param {*} transform
 * @param {*} request
 * @param {*} _opts
 * @param {(error: Error, statusCode: number, payload: Buffer, time: [number,number]) => void} cb
 */
function query(
  transform,
  request,
  _opts,
  cb = (error, statusCode, payload, time) => {},
) {
  const time = process.hrtime();
  const queries = new Queries();

  const acc = [];

  pipeline(
    request,
    queries,
    transform,
    new Writable({
      write(chunk, _enc, writeCallback) {
        acc.push(chunk);
        writeCallback();
      },
    }),
    error => {
      if (error) {
        cb(
          new MangerServiceError(error.message, 502),
          502,
          Buffer.from(''),
          time,
        );

        return;
      }

      cb(error, 200, Buffer.concat(acc), time);
    },
  );
}

module.exports = query;
