-module(load_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

server_spec() -> #{
  id => load_feeds,
  start => {load_feeds, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [load_feeds]
 }.

init([]) ->
  Procs = [server_spec()],
  {ok, {{rest_for_one, 1, 5}, Procs}}.
