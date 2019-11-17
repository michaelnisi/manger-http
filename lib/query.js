// query - query manger

const { Writable, pipeline } = require('readable-stream')
const { Queries } = require('manger')

/**
 * Queries the Manger cache piping `request` via Queries into `transform` 
 * and applies `cb` with an error and the accumulated result.
 * 
 * @param {*} transform 
 * @param {*} req 
 * @param {*} opts 
 * @param {*} cb 
 */
function query (transform, request, opts, cb = () => {}) {
  const time = process.hrtime()
  const queries = new Queries()

  let acc = []

  pipeline(
    request,
    queries,
    transform,
    new Writable({
      write(chunk, enc, cb) {
        acc.push(chunk)
        cb()
      }
    }),
    error => {
      const statusCode = err ? err.statusCode || 404 : 200
      const body = Buffer.concat(acc).toString()
  
      cb(error, statusCode, body, time)
    }
  )
}

exports = query