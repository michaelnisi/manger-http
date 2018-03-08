# mob - stress manger-http

A little Erlang program I use to stress one of my HTTP APIs—probably not very interesting if you are not me.

## Configuration

Use application configuration files to configure your *mob*.

### Example

```erlang
[
  {mob, [
    {host, "10.0.1.29"},
    {port, 8384},
    {feeds, "FEEDS"},
    {connections, 5}
  ]}
].
```

## License

[MIT](https://github.com/michaelnisi/mob/blob/master/LICENSE)
