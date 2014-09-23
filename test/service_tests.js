
var test = require('tap').test
  , Service = require('../')
  ;

test('start/stop', function (t) {
  var svc = new Service()
  svc.start(function (er) {
    t.ok(svc.db, 'should have db')
    t.ok(svc.manger, 'should have manger')
    t.ok(svc.server, 'should have server')
    svc.stop(function (er) {
      t.end()
    })
  })
})
