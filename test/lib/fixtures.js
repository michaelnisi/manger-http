'use strict'

const fs = require('fs')
const http = require('http')
const path = require('path')
const { debuglog } = require('util')

const debug = debuglog('fixtures')

exports.run = run

function mkArray (obj) {
  if (!obj) return
  return Array.isArray(obj) ? obj : [obj]
}

function resolve (file) {
  return path.resolve(__dirname, '..', 'data', file)
}

// Adjusts fixture, injecting defaults, etc.
//
// Mixing outbound and inbound like this is very confusing. These sloppy
// pattern matching tests are dangerous, for they are indicating a false
// impression of safety.
function normalize (fixture) {
  const req = fixture.request

  req.method = req.method || 'GET'

  let res = fixture.response

  if (Array.isArray(res)) {
    res = { payload: res }
  }

  res.statusCode = res.statusCode || 200

  const ttl = req.method === 'GET' ? 86400 : 3600

  res.headers = Object.assign({
    'cache-control': `max-age=${ttl}`,
    'connection': 'close',
    'content-type': 'application/json; charset=utf-8'
  }, res.headers || {})

  fixture.response = res

  return fixture
}

function readSync (file) {
  const p = resolve(file)
  const input = fs.readFileSync(p)
  const json = JSON.parse(input)
  const fixtures = mkArray(json)

  return fixtures.map(f => { return normalize(f) })
}

// Returns a fake remote server for Manger to hit. All remote server fixtures
// are merged into a single time series of test expectations. These are asserted
// in order by one server per file. Dedicated servers would be overkill, as
// these would all be on the same IP address (localhost) in any case.
function freshRemoteServer (fixture, t) {
  const remotes = fixture.reduce((acc, f) => {
    const r = mkArray(f.remote)

    return (r instanceof Array) ? acc.concat(r) : acc
  }, [])

  if (remotes.length === 0) return

  const fixtures = remotes.map((f) => {
    return (req, res) => {
      t.is(req.method, f.method || 'GET', 'should be method')
      t.is(req.url, f.url, 'should be URL')

      // If the fixture does not provide a status code, we’re assuming an error,
      // thus we’re bailing out with internal server error, 500.
      if (!f.statusCode) {
        res.writeHead(500)
        res.end()
        return
      }

      res.writeHead(f.statusCode, f.headers)

      if (f.file) {
        const p = resolve(f.file)

        t.comment(`streaming: ${p}`)
        fs.createReadStream(p).pipe(res)
      } else {
        res.end(f.payload)
      }
    }
  })

  const server = http.createServer((req, res) => {
    const f = fixtures.shift()

    f(req, res)
  })

  server.stop = (cb) => {
    t.is(fixtures.length, 0)
    server.close(cb)
  }

  return server
}

// Returns request options for our test target, the local manger-http API.
function httpRequestOpts (json) {
  let opts = Object.assign(Object.create(null), json)
  opts.port = opts.port || 1337

  return opts
}

// TODO: Wait on 202

function test (server, fixtures, t, cb) {
  const f = fixtures.shift()

  if (!f) return setTimeout(() => { cb() }, 1000)

  let wanted = f.response

  const opts = httpRequestOpts(f.request)

  t.test(f.title, (st) => {
    const req = http.request(opts, (res) => {
      st.match(res.headers, wanted.headers)

      const sc = wanted.statusCode || 200

      debug('status codes: (%s, %s )', res.statusCode, sc)

      st.is(res.statusCode, sc, 'should be status code')

      const body = wanted.payload

      let acc = ''

      res.on('error', (er) => {
        throw er
      })

      res.on('data', (chunk) => {
        acc += chunk
      })

      res.on('end', () => {
        const unbodied = req.method === 'HEAD' || sc === 304

        t.is(unbodied, acc === '', 'should withhold body')

        if (acc !== '') {
          const found = JSON.parse(acc)

          st.matches(found, body, 'should match payload')
        }

        // Delaying for non-committal response to make time for IO
        // before the next test.
        const delay = sc === 202 ? 500 : 0

        setTimeout(() => {
          st.end()
          test(server, fixtures, t, cb)
        }, delay)
      })

      res.resume()
    })

    const payload = f.request.payload

    if (payload) {
      req.write(JSON.stringify(payload))
    }

    req.end()
  })
}

function run (server, file, t, cb) {
  const fixtures = readSync(file)

  const remoteServer = freshRemoteServer(fixtures, t)

  function go () {
    server.start((er) => {
      if (er) return cb(er)
      test(server, fixtures, t, (er) => {
        if (er) throw er
        server.stop((er) => {
          if (er) throw er
          if (remoteServer) {
            remoteServer.stop((er) => {
              cb(er)
            })
          } else {
            cb()
          }
        })
      })
    })
  }

  if (remoteServer) {
    remoteServer.listen(8001, (er) => {
      if (er) throw er
      go()
    })
  } else {
    go()
  }
}
