%% mob_feeds - own the feeds ets table

-module(mob_feeds).
-behaviour(gen_server).

%% API

-export([start_link/0]).
-export([add/1]).
-export([rand/0]).

%% Callbacks

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(NAME, ?MODULE).

-record(state, {
    tid
  }).

-spec rand() -> string().

%% Returns a random feed URL.
rand() ->
  {_, Size} = lists:keyfind(size, 1, ets:info(feeds)),
  Key = rand:uniform(Size - 1),
  [{_, URL}] = ets:lookup(feeds, Key),
  URL.

start_link() ->
  gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

populate(Tab, Dev, Key) ->
  case io:get_line(Dev, "") of
    eof  ->
      ok;
    Line ->
      URL = string:strip(Line, right, $\n),
      ets:insert(Tab, {Key, URL}),
      populate(Tab, Dev, Key + 1)
  end.

lastKey(Tab) ->
  case ets:last(Tab) of
    {K, _} -> K;
    _ -> 0
  end.

%% Add lines in file as feed URLs to the feeds table.
add(File) ->
  {ok, Dev} = file:open(File, [read]),
  Last = lastKey(feeds),
  ok = populate(feeds, Dev, Last),
  file:close(Dev).

init([]) ->
  Tid = ets:new(feeds, [protected, named_table]),
  ok = add("./FEEDS"),
  {ok, #state{tid=Tid}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
