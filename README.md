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

All endpoints respond with JSON payloads and are offering gzip encoding.

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
- `version` The version of the package

### Retrieving and deleting feeds

#### A single feed

```
GET /feed/:uri
```

- `uri` The url-encoded URL of the feed

The response is an Array containing one `feed()`.

#### Remove a feed from the cache

```
DELETE /feed/:uri
```
- `uri` The url-encoded URL of the feed

Response

- `ok` `Boolean()`
- `id` `String()` The URL of the feed

or

- `error` `String()` The error message
- `reason` `String()` The reason for the error

#### List all cached feeds

```
GET /feeds
```

Response

An Array containing the URLs of all cached feeds.

To get the total number of feeds in the store, you might do something like:

```
curl -s localhost:8384/feeds | json -ga | wc -l
```

#### Selected feeds

```
POST /feeds
```

Where the request payload has to be `queries()`.

The response is an `Array()` of `feed()` objects.

### Retrieving entries

```
GET /entries/:uri
```

```
/POST entries
```

### Updating the cache

```
/GET update
```

```
/GET ranks
```

```
/PUT ranks
```

```
/DELETE ranks
```

## License

[MIT License](https://github.com/michaelnisi/manger-http/blob/master/LICENSE)
