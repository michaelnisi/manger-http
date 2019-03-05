'use strict'

const test = require('tap').test
const { isMatching } = require('../lib/respond')

test('matching Etags', (t) => {
  const ours = '5c1-/ad3b/EcoOacCXkzckvTg0PPon4'

  const triplets = [
    [{}, {}, false],
    [{'If-None-Match': '*'}, ours, true]
  ]

  t.plan(triplets.length)

  triplets.forEach(([headers, tag, wanted]) => {
    const found = isMatching(headers, tag)

    t.is(found, wanted)
  })
})
