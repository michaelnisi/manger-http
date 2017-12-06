const common = require('./lib/common')
const fixtures = require('./lib/fixtures')
const http = require('http')
const test = require('tap').test

function runFixtures (name, t) {
  const server = common.freshMangerServer()
  fixtures.run(server, name, t, (er) => {
    if (er) throw er
    t.end()
  })
}

test('root', (t) => {
  runFixtures('root.json', t)
})

test('not found', (t) => {
  runFixtures('notfound.json', t)
})

test('feed', (t) => {
  runFixtures('feed.json', t)
})

test('feeds', (t) => {
  runFixtures('feeds.json', t)
})

test('entries', (t) => {
  runFixtures('entries.json', t)
})

test('ranks', (t) => {
  runFixtures('ranks.json', t)
})

test('abort', (t) => {
  const server = common.freshMangerServer()
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
