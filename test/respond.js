'use strict'

const test = require('tap').test
const { isMatching } = require('../lib/respond')

test('matching Etags', (t) => {
  const ours = 'xxx'

  const triplets = [
    [{}, {}, false],
    [{'If-None-Match': '*'}, ours, true],
    [{'If-None-Match': '"abc", "def"'}, ours, false],
    [{'If-None-Match': '"xxx", "def"'}, ours, true],
    [{'If-None-Match': 'xxx, "def"'}, ours, true],
    [{'If-None-Match': 'xxx'}, ours, true],
    [{'If-None-Match': 'W/"67ab43", "54ed21", "7892dd"'}, ours, false],
    [{'If-None-Match': 'W/"67ab43", "xxx", "7892dd"'}, ours, true],
    [{'If-None-Match': 'W/"xxx", "67ab43", "7892dd"'}, ours, true],
    [{'If-None-Match': '"5c1-/ad3b/EcoOacCXkzckvTg0PPon4"'}, '"5c1-/ad3b/EcoOacCXkzckvTg0PPon4"', true],
    [{'if-none-match': 'xxx'}, ours, true]
  ]

  t.plan(triplets.length)

  triplets.forEach(([headers, tag, wanted]) => {
    const found = isMatching(headers, tag)

    t.is(found, wanted)
  })
})
