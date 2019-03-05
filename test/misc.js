const mangerHTTP = require('../')
const test = require('tap').test
const common = require('./lib/common')

test('constructor', (t) => {
  const f = mangerHTTP
  const opts = () => {
    return {
      location: common.freshName()
    }
  }
  t.plan(2)
  t.ok(f(opts()) instanceof mangerHTTP.MangerService)
  t.is(f(opts()).port, 8384)
  t.end()
})

test('ok', (t) => {
  const f = mangerHTTP.ok
  const wanted = [
    false,
    false,
    false
  ]
  const found = [
    f(undefined),
    f(new Error()),
    f(new Error('hullo'))
  ]
  ;['client error',
    'not deleted',
    'Parse Error',
    'quaint HTTP status',
    'query error',
    'request error'
  ].forEach((str) => {
    found.push(f(new Error(str)))
    wanted.push(true)
  })
  t.plan(wanted.length)
  found.forEach((it) => {
    t.is(it, wanted.shift())
  })
})
