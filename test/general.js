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

test('home', function (t) {
  t.plan(2)
  http.get('http://localhost:1337', function (res) {
    t.is(res.statusCode, 200)
    var buf = ''
    res.on('data', function (chunk) {
      buf += chunk
    })
    res.on('end', function () {
      var found = JSON.parse(buf)
      var wanted = {
        name: 'manger',
        version: '1.0.1'
      }
      t.same(wanted, found)
    })
  })
})

test('not found', function (t) {
  t.plan(2)
  http.get('http://localhost:1337/x', function (res) {
    t.is(res.statusCode, 404)
    var buf = ''
    res.on('data', function (chunk) {
      buf += chunk
    })
    res.on('end', function () {
      var found = JSON.parse(buf)
      var wanted = {
        error: 'not found',
        reason: '/x is not an endpoint'
      }
      t.same(wanted, found)
    })
  })
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
