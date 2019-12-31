// routes - route requests
// @ts-check

const assert = require('assert');
const {URL} = require('url');
const {StringDecoder} = require('string_decoder');
const query = require('./query');
const {Query} = require('manger');
const {pipeline, Writable} = require('readable-stream');
const http = require('http'); // eslint-disable-line
const {MangerServiceError} = require('./meta');

module.exports = {
  deleteFeed,
  entries,
  entriesOfFeed,
  feed,
  feeds,
  flushCounter,
  list,
  ranks,
  resetRanks,
  root,
};

function feeds(req, _res, opts, cb) {
  const s = opts.manger.feeds();

  query(s, req, opts, cb);
}

function urlFromParams({url}) {
  return typeof url === 'string' ? unescape(url) : null;
}

/**
 * Handles requests for single feeds or entries of single feeds. The purpose
 * of these extra routes is to enhance external caching by using these clearcut
 * GET requests for singly things, which are quite common use cases with our
 * clients.
 *
 * @param {*} transform The `manger` transform stream.
 * @param {http.IncomingMessage} _req
 * @param {http.ServerResponse} _res
 * @param {object} opts
 * @param {(error: Error, statusCode: number, payload: Buffer, time: [number,number]) => void} cb
 */
function single(
  transform,
  _req,
  _res,
  opts,
  cb = (error, statusCode, payload, time) => {},
) {
  const t = process.hrtime();

  let q;
  const acc = [];
  const {params, log} = opts;

  try {
    const url = urlFromParams(params);
    q = new Query({url});
  } catch (error) {
    log.trace({error, params}, 'bad request');
    cb(new MangerServiceError(error.message, 400), 400, Buffer.from(''), t);
  }

  pipeline(
    transform,
    new Writable({
      write(chunk, _enc, writeCallback) {
        acc.push(chunk);
        writeCallback();
      },
    }),
    error => {
      cb(error, 200, Buffer.concat(error ? [] : acc), t);
    },
  );

  transform.end(q);
}

function feed(req, res, opts, cb) {
  const s = opts.manger.feeds();

  single(s, req, res, opts, cb);
}

function entriesOfFeed(req, res, opts, cb) {
  const s = opts.manger.entries();

  single(s, req, res, opts, cb);
}

function deleteFeed(_req, _res, opts, cb) {
  const t = process.hrtime();
  const {params} = opts;
  const uri = urlFromParams(params);
  const cache = opts.manger;

  cache.remove(uri, function cacheRemoveHandler(er) {
    let sc = 200;
    let buf;
    let error;

    if (er) {
      const problem = 'not deleted';

      let reason;

      if (er.notFound) {
        reason = uri + ' not cached';
        sc = 404;
      } else {
        sc = 500;
      }

      buf = JSON.stringify({
        error: problem,
        reason,
      });

      error = new Error(problem + ': ' + reason);
    } else {
      buf = JSON.stringify({ok: true, id: uri});
    }

    cb(error, sc, buf, t);
  });
}

function entries(req, _res, opts, cb) {
  const s = opts.manger.entries();

  query(s, req, opts, cb);
}

function urls(readable, _opts, cb) {
  assert(cb, 'callback required');

  const sc = 200;
  const t = process.hrtime();
  const acc = [];

  function onend(er) {
    readable.removeListener('data', ondata);
    readable.removeListener('error', onerror);
    readable.removeListener('end', onend);

    const payload = JSON.stringify(acc);

    if (cb) {
      cb(null, sc, payload, t);
    }
  }

  function onerror(er) {
    onend(er);
  }

  const decoder = new StringDecoder('utf8');

  function decode(chunk) {
    return decoder.write(chunk);
  }

  function ondata(chunk) {
    if (!acc) {
      return;
    }

    acc.push(chunk instanceof Buffer ? decode(chunk) : chunk);
  }

  readable.on('data', ondata);
  readable.on('error', onerror);
  readable.on('end', onend);
}

function list(_req, _res, opts, cb) {
  const s = opts.manger.list();

  urls(s, opts, cb);
}

function root(_req, _res, opts, cb) {
  const payload = JSON.stringify({
    name: 'manger',
    version: opts.version,
  });

  return cb ? cb(null, 200, payload) : null;
}

function limit(searchParams) {
  if (!searchParams) {
    return null;
  }

  const l = Number(searchParams.get('limit'));

  return l > 0 ? l : null;
}

function ranks(req, _res, opts, cb) {
  const {searchParams} = new URL(req.url, 'no://');
  const s = opts.manger.ranks(limit(searchParams));

  return urls(s, opts, cb);
}

const OK = JSON.stringify({ok: true});

function resetRanks(_req, _res, opts, cb) {
  cb(null, 202, OK);

  const cache = opts.manger;

  cache.resetRanks(function resetRanksHandler(er) {
    if (er) {
      const error = new Error('could not reset ranks: ' + er.message);

      opts.log.warn(error);
    }
  });
}

function flushCounter(_req, _res, opts, cb) {
  cb(null, 202, OK);

  const cache = opts.manger;
  const log = opts.log;

  cache.flushCounter((er, count) => {
    if (er) {
      log.warn(er);
    }
    log.debug({count}, 'flushed');
  });
}
