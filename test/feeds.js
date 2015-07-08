var test = require('tap').test
var rimraf = require('rimraf')
var http = require('http')
var common = require('./lib/common')

var server = common.freshServer()

test('setup', function (t) {
  t.plan(1)
  server.start(function (er) {
    if (er) throw er
    t.pass('should start')
  })
})

test('feed', function (t) {
  t.plan(3)
  var uri = encodeURIComponent('http://www.newyorker.com/feed/posts')
  var opts = {
    host: server.host,
    port: server.port,
    path: '/feed/' + uri
  }
  var req = http.request(opts, function (res) {
    t.is(res.statusCode, 200)
    var buf = ''
    res.on('data', function (chunk) {
      buf += chunk
    })
    res.on('end', function () {
      var found = JSON.parse(buf)
      t.is(found.length, 1)
      t.ok(found[0].title)
      // As the content is not under our control, it does not make sense
      // to test more than mere existance.
    })
  })
  req.end()
})

test('update', function (t) {
  t.plan(2)
  var opts = {
    host: server.host,
    port: server.port,
    method: 'PUT',
    path: '/feeds'
  }
  var req = http.request(opts, function (res) {
    t.is(res.statusCode, 202)
    var buf = ''
    res.on('data', function (chunk) {
      buf += chunk
    })
    res.on('end', function () {
      var found = JSON.parse(buf)
      var wanted = { ok: true }
      t.same(found, wanted)
    })
  })
  req.end()
})

test('teardown', function (t) {
  t.plan(2)
  server.stop(function (er) {
    if (er) throw er
    t.pass('should stop')
    var f = server.location
    rimraf(f, function (er) {
      if (er) throw er
      t.pass('should remove ' + f)
    })
  })
})
