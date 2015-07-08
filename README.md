# manger-http - HTTP API for manger

**manger-http** is a HTTP/1.1 API for caching RSS feeds.

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

All endpoints return JSON and may apply gzip encoding.

### Types

#### void()

`undefined | null`

#### str()

An optional string.

`String() | void()`


### feed()

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

### General

`/GET`

General information about the server.

Response

- `name` The name of the server
- `version` The version of the package

### Retrieving and deleting feeds

```
/GET feed/:uri
```

Request information about a single feed.

- `uri` `str`  The URL of the feed

Response:

The response is an Array containing one `feed()`.

```
/DELETE feed/:uri
```

- uri The URL of the feed

```
/GET feeds
```

```
/POST feeds
```

### Retrieving entries

```
/GET entries/:uri
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
