-module(mob_stream).

-export([stream/1]).

queries(N) ->
  queries(N, []).
queries(0, Queries) ->
 jsx:encode(Queries);
queries(N, Queries) ->
  URL = list_to_binary(mob_feeds:rand()),
  Query = #{<<"url">> => URL},
  queries(N-1, [Query|Queries]).

uri_encoded_rand() ->
  http_uri:encode(mob_feeds:rand()).

stream(root, C) ->
  gun:get(C, "/");
stream(cached_feeds, C) ->
  gun:get(C, "/feeds");
stream(ranks, C) ->
  gun:get(C, "/ranks");
stream(entries, C) ->
  N = rand:uniform(10),
  Body = queries(N),
  gun:post(C, "/entries", [
    {<<"content-type">>, "application/json"}
  ], Body);
stream(feeds, C) ->
  N = rand:uniform(10),
  Body = queries(N),
  gun:post(C, "/feeds", [
    {<<"content-type">>, "application/json"}
  ], Body);
stream(feed, C) ->
  P = "/feed/" ++ uri_encoded_rand(),
  gun:get(C, P);
stream(entries_of_feed, C) ->
  P = "/entries/" ++ uri_encoded_rand(),
  gun:get(C, P);
stream(delete, C) ->
  P = "/feed/" ++ uri_encoded_rand(),
  gun:delete(C, P);
stream(update, C) ->
  gun:put(C, "/feeds", []);
stream(update_ranks, C) ->
  gun:put(C, "/ranks", []).

rand_stream(Streams) ->
  N = rand:uniform(length(Streams)),
  lists:nth(N, Streams).

%% Rarely
streams(N) when N < 5 ->
  rand_stream([update, update_ranks, delete]);
%% Sometimes
streams(N) when N < 25 ->
  rand_stream([root, ranks, cached_feeds]);
%% Often
streams(_) ->
  rand_stream([feeds, entries, feed, entries_of_feed]).

%% Issue random request on the specified Gun connection and return the stream
%% reference.
stream(C) ->
  N = rand:uniform(250),
  S = streams(N),
  stream(S, C).
