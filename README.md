# manger-http - cache feeds

**manger-http** is an HTTP/1.1 API for caching RSS feeds.

## API

### Responses

An exemplary response header of this API.

```
HTTP/1.1 200 OK
Cache-Control: must-revalidate, max-age=86400
Content-Type: application/json; charset=utf-8
Content-Length: 141
Latency: 8478003
Content-Encoding: gzip
Date: Sat, 04 Jul 2015 14:08:40 GMT
Connection: keep-alive
```

All endpoints respond with JSON payloads and offer gzip encoding.

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

#### Remove a feed from the cache

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

#### List all cached feeds

```
GET /feeds
```

An Array containing the URLs of all feeds in the cache.

To count the feeds in the store, you could do something like:

```
curl -s localhost:8384/feeds | json -ga | wc -l
```

#### Selected feeds

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

#### Update all feeds

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

#### Update the ranks

Request counts are not updated live, but are kept in memory. This operation flushes these changes and updates the rank index.

```
/PUT ranks
```

#### Reset ranks

Reset ranks by deleting the rank index.

```
/DELETE ranks
```

## License

[MIT License](https://github.com/michaelnisi/manger-http/blob/master/LICENSE)
