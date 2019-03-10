# manger-http - cache feeds

**manger-http** implements an HTTP/1.1 API for caching RSS feeds.

## API

### Responses

An exemplary response header of this API.

```
HTTP/1.1 200 OK
Cache-Control: max-age=86400
Content-Length: 28502
Content-Type: application/json; charset=utf-8
Surrogate-Control: max-age=604800
Backend-Latency: 25.19
ETag: "6f56-4rMTzq5+UvI8vHbtRxfpEHr60mc"
Date: Fri, 22 Feb 2019 15:01:45 GMT
Connection: keep-alive
```

All routes respond with JSON payloads and offer gzip encoding. The server supports `If-None-Match` [conditional requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Conditional_requests) and `HEAD` requests. All responses contain ETag headers. If internal latency exceeds 20 milliseconds, a `Backend-Latency` header is added.

If there is no response payload you get:

```json
{
  "ok": true
}
```

Errors are JSON too, for example:

```json
{
  "error": "not found",
  "reason": "/x is not an endpoint"
}
```

### Types

#### void()

`undefined | null`

#### str()

An optional string.

`String() | void()`

#### feed()

Meta information about a feed.

- `author` `str()`
- `copyright` `str()`
- `feed` `str()`
- `id` `str()`
- `image` `str()`
- `language` `str()`
- `link` `str()`
- `payment` `str()`
- `subtitle` `str()`
- `summary` `str()`
- `title` `str()`
- `ttl` `str()`
- `updated` `str()`

### enclosure()

A related resource of an `entry()`.

- `href` `str()`
- `length` `str()`
- `type` `str()`

### entry()

An individual entry.

- `author` `str()`
- `enclosure enclosure() | void()`
- `duration` `str()`
- `feed` `str()`
- `id` `str()`
- `image` `str()`
- `link` `str()`
- `subtitle` `str()`
- `summary` `str()`
- `title` `str()`
- `updated` `str()`

#### date()

Anything `Date()` can parse.

#### query()

A query to fetch a feed or entries of a feed limited by a time range.

- `url` `String()` The URL of the feed
- `since` `date()` The date of the oldest entry you want

#### queries()

An `Array()` of `query()` objects.

### General

#### The version of the API

`GET /`

Response

- `name` The name of the server
- `version` The version of the API (the package version)

### Retrieving and deleting feeds

#### A single feed

```
GET /feed/:uri
```

- `:uri` The url-encoded URL of the feed

The response is an `Array()` containing the requested `feed()` or an empty `Array()` if the feed could not be found.

#### Removing feeds from the cache

```
DELETE /feed/:uri
```
- `:uri` The url-encoded URL of the feed

The response is a confirmation:

- `ok` `Boolean()`
- `id` `String()` The URL of the feed

or

- `error` `String()` The error message
- `reason` `String()` The reason for the error

#### Listing all cached feeds

```
GET /feeds
```

An Array containing the URLs of all feeds in the cache.

To count the feeds in the store, you could do something like:

```
curl -s localhost:8384/feeds | json -ga | wc -l
```

#### Selecting feeds

```
POST /feeds
```

Where the message body has to be `queries()`. Note that time ranges (defined by `since`) are ignored here.

The response is an `Array()` of `feed()` objects.

### Retrieving entries

#### All entries of a specific feed

```
GET /entries/:uri
```

- `:uri` The url-encoded URL of the feed

Responds with an `Array()` of `entry()` objects or an empty `Array()`. These `GET` APIs are to facilitate caching, thus they don’t allow for time ranged requests, which would just belittle chances of hitting the cache.

#### Selected entries of any feed

```
/POST entries
```

As in `/POST feeds`, the message body has to be `queries()`, in this case though, time ranges filter entries.

The response is an `Array()` of `entry()` objects.

### Updating the cache

#### Updating all feeds

```
/PUT feeds
```

Update all feeds in the store in ranked order—thus the rank index gets refreshed first. All this IO can make this operation run significantly long. To keep track of these, there is an info level log when it completes.

The immediate response is a `202 Accepted` with `{ "ok": true }`.

#### All feed URLs in ranked order

```
/GET ranks
```

An `Array()` with all feed URLs in the cache ordered by number of requests.

For the top-ten you could do:

```
curl -s localhost:8384/ranks | json -ga | head
```

#### Updating ranks

Request counts are not updated live, but are kept in memory. This operation flushes these changes and updates the rank index.

```
/PUT ranks
```

#### Resetting ranks

Reset ranks by deleting the rank index.

```
/DELETE ranks
```

## Testing

Of course, there are some unit tests.

```
$ npm t
```

You can start the server with npm.

```
$ npm start
```

And start tackling it with curl.

```
$ curl -v localhost:8384/feed/http%3A%2F%2Fscriptnotes.net%2Frss
```

In `./tools/load`, you find a little Erlang/OTP app for putting some load on the server. It’s based on [mob](https://github.com/michaelnisi/mob) and is good for smoke testing after changes were applied to the system.

```
$ erl -pa ebin deps/*/ebin
```

```
1> load:start().
ok
2> load:hit(5).
[{ok,<0.127.0>},
 {ok,<0.125.0>},
 {ok,<0.123.0>},
 {ok,<0.121.0>},
 {ok,<0.119.0>}]
3>
```

This would hit the server with random requests on five connections. Adjust log levels or run with `NODE_DEBUG=manger-http node start.js` to see what’s going on.

## Installing

Clone this repo and `npm install`.

## Operating

To run the server on your local machine you can start **manger-http** with:

```
$ npm start
```

The `./setup` directory contains scripts for scheduling updates and service management on [SmartOS](https://www.joyent.com/smartos).

With the Node Dtrace provider, [Bunyan](https://github.com/trentm/node-bunyan) enables dynamic tracing with [Dtrace](http://dtrace.org). Make sure it gets installed `NODE_DTRACE_PROVIDER_REQUIRE=hard npm i`.

```
$ bunyan -p $(pgrep node)
```

## License

[MIT License](https://github.com/michaelnisi/manger-http/blob/master/LICENSE)
