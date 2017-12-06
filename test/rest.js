const common = require('./lib/common')
const fixtures = require('./lib/fixtures')
const http = require('http')
const test = require('tap').test

test('root', (t) => {
  fixtures.run('root.json', t, (er) => {
    if (er) throw er
    t.end()
  })
})

test('not found', (t) => {
  fixtures.run('notfound.json', t, (er) => {
    if (er) throw er
    t.end()
  })
})

test('feed', (t) => {
  fixtures.run('feed.json', t, (er) => {
    if (er) throw er
    t.end()
  })
})

test('feeds', (t) => {
  fixtures.run('feeds.json', t, (er) => {
    if (er) throw er
    t.end()
  })
})

test('entries', (t) => {
  fixtures.run('entries.json', t, (er) => {
    if (er) throw er
    t.end()
  })
})

test('ranks', (t) => {
  fixtures.run('ranks.json', t, (er) => {
    if (er) throw er
    t.end()
  })
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
