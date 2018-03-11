%% load - stress manger-http

-module(load).

-export([
  start/0,
  stop/0,
  hit/1
]).

start() ->
  {ok, _} = application:ensure_all_started(mob),
  {ok, _} = application:ensure_all_started(jsx),
  application:start(?MODULE).

stop() ->
  ok = application:stop(mob),
  ok = application:stop(?MODULE).

hit(J) ->
  mob:add(fun load_stream:stream/1, J).
