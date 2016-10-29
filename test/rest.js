var common = require('./lib/common')
var fs = require('fs')
var http = require('http')
var path = require('path')
var test = require('tap').test
var nock = require('nock')

function parse (file) {
  var p = path.resolve(__dirname, 'data', file)
  var input = fs.readFileSync(p)
  var json = JSON.parse(input)
  return json instanceof Array ? json : [json]
}

test('abort', (t) => {
  var server = common.freshServer()
  server.start((er) => {
    if (er) throw er
    const req = http.get('http://localhost:1337/', () => {})
    req.on('error', (er) => {
      t.is(er.code, 'ECONNRESET')
      server.stop((er) => {
        if (er) throw er
        t.end()
      })
    })
    process.nextTick(() => {
      req.abort()
    })
  })
})

test('basic REST API', function (t) {
  var p = path.resolve(__dirname, 'data')
  var files = fs.readdirSync(p)
  var scopes = []
  var tests = files.reduce(function (acc, file) {
    if (path.extname(file) !== '.json') {
      return acc
    }
    var children = parse(file)

    function go (children, cb) {
      var child = children.shift()
      if (!child) {
        return cb()
      }
      var remote = child.remote
      if (remote) {
        var remotes = remote instanceof Array ? remote : [remote]
        remotes.forEach(function (r) {
          var scope = nock(r.host)
          var m = r.method || 'GET'
          var sc = r.statusCode || 200
          scope.intercept(r.path, m).reply(sc, function () {
            t.pass('should be hit')
            if (!r.file) return null
            var p = path.resolve(__dirname, 'data', r.file)
            return fs.createReadStream(p)
          }, r.headers)
          scopes.push(scope)
        })
      }
      var opts = child.request
      var response = child.response
      var sc = response.statusCode
      var req = http.request(opts, function (res) {
        t.is(res.statusCode, sc || 200)
        var buf = ''
        res.on('data', function (chunk) {
          buf += chunk
        })
        res.on('end', function () {
          var found = JSON.parse(buf)
          var wanted = response.payload || response
          t.match(found, wanted)
          if (sc === 202) {
            setTimeout(function () {
              go(children, cb)
            }, 500)
          } else {
            go(children, cb)
          }
        })
      })
      var payload = opts.payload
      if (payload) {
        req.end(JSON.stringify(payload))
      } else {
        req.end()
      }
    }
    var closure = function (cb) {
      t.comment(file)
      var server = common.freshServer()
      server.start(function (er) {
        if (er) throw er
        go(children, function () {
          server.stop(function (er) {
            if (er) throw er
            cb()
          })
        })
      })
    }
    acc.push(closure)
    return acc
  }, [])

  function run (tests) {
    var f = tests.shift()
    if (f) {
      f(function () {
        run(tests)
      })
    } else {
      scopes.forEach(function (scope) {
        t.ok(scope.isDone())
      })
      t.is(tests.length, 0, 'should test all')
      t.end()
    }
  }
  run(tests)
})
