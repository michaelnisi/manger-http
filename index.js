// manger-http - serve podcast feeds with persistent caching
// @ts-check

const HttpHash = require('http-hash');
const fs = require('fs');
const http = require('http');
const httpMethods = require('http-methods/method');
const {Manger, createLevelDB} = require('manger');
const mkdirp = require('mkdirp');
const path = require('path');
const update = require('./lib/update');
const {
  crash,
  createServiceError,
  createLogger,
  MangerServiceError,
} = require('./lib/meta');
const {respond, respondAfterError} = require('./lib/respond');
const assert = require('assert');

const {
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
} = require('./lib/routes');

/**
 * Returns the package manifest object.
 */
function readPackage() {
  const p = path.join(__dirname, 'package.json');
  const data = fs.readFileSync(p);

  return JSON.parse(data.toString());
}

/**
 * Additional request state.
 *
 * @param {*} log
 * @param {*} manger
 * @param {*} params
 * @param {*} splat
 * @param {*} version
 */
function ReqOpts(log, manger, params, splat, version) {
  this.log = log;
  this.manger = manger;
  this.params = params;
  this.splat = splat;
  this.version = version;
}

/**
 * Debug logs cache hits.
 *
 * @param {*} qry
 */
function hitHandler(qry) {
  this.log.debug(qry, 'hit');
}

/**
 * @param {Error} error
 */
function errorHandler(error) {
  const er = createServiceError(error);
  const {statusCode} = er;

  if (statusCode !== 500) {
    this.log.warn(er.message);

    return;
  }

  crash(er, this.log);
}

class MangerService {
  /**
   * Creates a new Manger Service.
   *
   * @param {{location: string, port: number, log: any, cacheSize: number}} props
   */
  constructor({location, port, log, cacheSize}) {
    this.location = location || '/tmp/manger-http';
    this.port = port || 8384;
    this.log = createLogger(log);
    this.cacheSize = cacheSize || 16 * 1024 * 1024;
    this.hash = HttpHash();
    const {version} = readPackage();
    this.version = version;
    this.manger = null;
    this.server = null;

    mkdirp.sync(this.location);
  }

  /**
   * Handles request and response passing a callback into the route handler.
   */
  handleRequest(req, res, cb) {
    if (typeof cb !== 'function') {
      throw new Error('callback required to handle request');
    }

    const route = this.hash.get(req.url);
    const {handler} = route;

    if (!handler) {
      return cb(new MangerServiceError('not found', 404));
    }

    const opts = new ReqOpts(
      this.log,
      this.manger,
      route.params,
      route.splat,
      this.version,
    );

    return handler(req, res, opts, cb);
  }

  setRoutes() {
    const {hash} = this;

    function set(name, handler) {
      assert(typeof handler === 'object');
      hash.set(name, httpMethods(handler));
    }

    set('/', {
      GET: root,
      HEAD: root,
    });

    set('/entries', {
      HEAD: entries,
      POST: entries,
    });

    set('/entries/:url', {
      GET: entriesOfFeed,
      HEAD: entriesOfFeed,
    });

    set('/feeds', {
      GET: list,
      HEAD: list,
      POST: feeds,
      PUT: update.bind(this),
    });

    set('/feed/:url', {
      DELETE: deleteFeed,
      GET: feed,
      HEAD: feed,
    });

    set('/ranks', {
      DELETE: resetRanks,
      GET: ranks,
      HEAD: ranks,
      PUT: flushCounter,
    });
  }

  start(cb) {
    const {log} = this;

    const info = {
      version: this.version,
      location: this.location,
      cacheSize: this.cacheSize,
    };

    log.info(info, 'starting');

    const db = createLevelDB(this.location, this.cacheSize);
    const cache = new Manger(db, {
      isEntry: entry => {
        if (entry.enclosure) {
          return true;
        }

        log.trace(entry.url, 'invalid entry');

        return false;
      },
      isFeed: _feed => true,
    });

    this.errorHandler = errorHandler.bind(this);

    cache.on('error', this.errorHandler);

    this.hitHandler = hitHandler.bind(this);

    cache.on('hit', this.hitHandler);

    this.manger = cache;

    this.setRoutes();

    const onrequest = (req, res) => {
      log.info({method: req.method, url: req.url}, 'request');

      this.handleRequest(req, res, (error, statusCode, payload, time) => {
        if (error) {
          respondAfterError(error, {req, res, payload, time, log});
          return;
        }

        respond(req, res, statusCode, payload, time, log);
      });
    };

    const server = http.createServer(onrequest);
    const {port} = this;

    server.listen(port, er => {
      log.info('listening: %s', port);

      if (cb) {
        cb(er);
      }
    });

    server.on('clientError', (_er, socket) => {
      // Reverse proxy default health checking connects and disconnects every
      // couple of seconds, producing a heartbeat here, hence the discreet logging.
      socket.once('close', () => {
        log.trace('heartbeat');
      });
      if (!socket.destroyed) {
        socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
      }
    });

    this.server = server;

    log.trace(this, 'started');
  }
}

module.exports = exports = MangerService;
