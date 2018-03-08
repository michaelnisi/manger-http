%% mob - stress manger-http

-module(mob).

-export([start/0]).
-export([stop/0]).

start() ->
  {ok, _} = application:ensure_all_started(gun),
  application:start(?MODULE).

stop() ->
  ok = application:stop(?MODULE),
  application:stop(gun).
