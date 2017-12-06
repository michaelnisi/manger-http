const common = require('./common')
const fs = require('fs')
const http = require('http')
const path = require('path')

exports.run = run

function mkArray (obj) {
  if (!obj) return
  return obj instanceof Array ? obj : [obj]
}

function resolve (file) {
  return path.resolve(__dirname, '..', 'data', file)
}

function readSync (file) {
  const p = resolve(file)
  const input = fs.readFileSync(p)
  const json = JSON.parse(input)
  return mkArray(json)
}

// Returns a fake remote server for Manger to hit. All remote server fixtures
// are merged into a single time series of test expectations. These are asserted
// in order by one server per file. Dedicated servers would be overkill, as
// these would all be on the same IP address (localhost) in any case.
function freshRemoteServer (fixture, t) {
  const remotes = fixture.reduce((acc, f) => {
    const r = mkArray(f.remote)
    if (r instanceof Array) return acc.concat(r)
    return acc
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

function test (server, fixtures, t, cb) {
  const f = fixtures.shift()
  if (!f) {
    return setTimeout(() => {
      cb()
    }, 1000)
  }

  const wanted = f.response
  const opts = httpRequestOpts(f.request)

  t.test(f.title, (st) => {
    const req = http.request(opts, (res) => {
      const sc = wanted.statusCode || 200
      const body = wanted.payload || wanted

      st.is(res.statusCode, sc, 'should be status code')
      let acc = ''
      res.on('error', (er) => {
        throw er
      })
      res.on('data', (chunk) => {
        acc += chunk
      })
      res.on('end', () => {
        const found = JSON.parse(acc)
        st.matches(found, body, 'should match payload')
        st.end()
        test(server, fixtures, t, cb)
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

function run (file, t, cb) {
  const fixtures = readSync(file)
  const server = common.freshMangerServer()

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
