-module(mob_request).
-behaviour(gen_fsm).

-export([code_change/4]).
-export([handle_info/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([init/1]).
-export([terminate/3]).

-record(state, {
  protocol,
  host,
  port,
  connection,
  stream
}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API

-export([start_link/1]).

%% This is a perpetual motion machine, it just needs to be started.
start_link(Opts) ->
  gen_fsm:start_link(?MODULE, Opts, []).

%% Callbacks

connect(Host, Port) ->
  {ok, ConnPid} = gun:open(Host, Port),
  #state{
    host=Host,
    port=Port,
    connection=ConnPid
  }.

init({Host, Port}) ->
  process_flag(trap_exit, true),
  State = connect(Host, Port),
  {ok, connecting, State}.

terminate(_Reason, _StateName, State) ->
  C = State#state.connection,
  gun:shutdown(C).

request(State) ->
  C = State#state.connection,
  S = mob_stream:stream(C),
  Next = State#state{stream=S},
  {next_state, requesting, Next}.

maybe_cancel(C, S) ->
  case rand:uniform(500) of
    1 -> gun:cancel(C, S);
    _ -> no
  end.

%% TODO: Collect statistics
%% TODO: Should retry
handle_info({gun_up, _, _}, _, State) ->
  request(State);
handle_info({gun_response, _, _, fin, _, _}, _, State) ->
  {next_state, requesting, State};
handle_info({gun_response, C, S, nofin, _, _}, _, State) ->
  case maybe_cancel(C, S) of
    ok -> {stop, normal, State};
    _ -> {next_state, receiving, State}
  end;
handle_info({gun_data, _, _, nofin, _}, _, State) ->
  {next_state, receiving, State};
handle_info({gun_data, _, _, fin, _}, _, State) ->
  Time = rand:uniform(5000) + 1000,
  timer:sleep(Time),
  request(State);
handle_info(Message, _StateName, State) ->
  {stop, Message, State}.

handle_event(_Event, _StateName, State) ->
  {stop, unhandled_event, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
  {stop, unhandled_event, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
