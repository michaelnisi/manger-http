-module(mob_app).
-behaviour(application).

%% API
-export([start/2]).
-export([stop/1]).

start(_Type, Args) ->
  dbg:tracer(),
  dbg:p(all, c),
  mob_sup:start_link(Args).

stop(_State) ->
	ok.
