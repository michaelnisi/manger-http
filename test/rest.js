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

function countTests (children) {
  return children.length * 2 + children.reduce(function (a, child) {
    var remote = child.remote
    if (remote) {
      var remotes = remote instanceof Array ? remote : [remote]
      return a + remotes.length * 2
    } else {
      return a
    }
  }, 0)
}

test('basic REST API', { bail: true }, function (t) {
  var p = path.resolve(__dirname, 'data')
  var files = fs.readdirSync(p)
  var scopes = []
  var count = 0
  var tests = files.reduce(function (acc, file) {
    if (path.extname(file) !== '.json') {
      return acc
    }
    var children = parse(file)
    count += countTests(children)

    function go (children, cb) {
      var child = children.shift()
      if (!child) {
        cb()
        return
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
            }, 100)
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
    }
  }
  t.plan(++count)
  t.is(count, 55, 'should be anal')
  run(tests)
})

