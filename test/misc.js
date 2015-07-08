var manger_http = require('../')
var test = require('tap').test
var common = require('./lib/common')

test('constructor', function (t) {
  var f = manger_http
  function opts () {
    return {
      location: common.freshName()
    }
  }
  t.plan(2)
  t.ok(f(opts()) instanceof manger_http.MangerService)
  t.is(f(opts()).port, 8384)
  t.end()
})

test('factor', function (t) {
  var f = manger_http.factor
  var wanted = [
    1,
    10,
    500,
    500
  ]
  var found = [
    f(0),
    f(100),
    f(5000),
    f(10000)
  ]
  t.plan(wanted.length)
  found.forEach(function (it) {
    t.is(it, wanted.shift())
  })
})

test('ok', function (t) {
  var f = manger_http.ok
  var wanted = [
    false,
    false,
    false
  ]
  var found = [
    f(undefined),
    f(new Error()),
    f(new Error('hullo'))
  ]
  ;['client error',
    'invalid query',
    'not deleted',
    'parse error',
    'quaint HTTP status',
    'query error',
    'request error'
  ].forEach(function (str) {
    found.push(f(new Error(str)))
    wanted.push(true)
  })
  t.plan(wanted.length)
  found.forEach(function (it) {
    t.is(it, wanted.shift())
  })
})
