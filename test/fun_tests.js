
var test = require('tap').test
  , manger_http = require('../')
  ;

test('constructor', function (t) {
  var f = manger_http
  t.plan(2)
  t.ok(f() instanceof manger_http.MangerService)
  t.is(f().port, 8384)
  t.end()
})
