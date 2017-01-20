-module(mob_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(NAME, ?MODULE).

start_link(Opts) ->
  supervisor:start_link({local, ?NAME}, ?MODULE, Opts).

sup_flags() -> #{
  strategy => rest_for_one,
  intensity => 10,
  period => 5
}.

server_spec() -> #{
  id => mob_feeds,
  start => {mob_feeds, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [mob_feeds]
}.

client_spec(Host, Port, Id) -> #{
  id => Id,
  start => {mob_request, start_link, [{Host, Port}]},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [mob_request]
}.

child_specs() ->
  {ok, Host} = application:get_env(mob, host),
  {ok, Port} = application:get_env(mob, port),
  {ok, M} = application:get_env(mob, connections),
  Clients = [client_spec(Host, Port, N) || N <- lists:seq(1, M)],
  [server_spec()|Clients].

init(_) ->
  Specs = child_specs(),
  {ok, {sup_flags(), Specs}}.
