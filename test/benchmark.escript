#!/usr/bin/env escript

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-mode(compile).

main([Sz0, KeySz0, ValSz0]) ->
    Sz = to_int(Sz0),
    KeySize = to_int(KeySz0),
    ValSize = to_int(ValSz0),
    if Sz > 0 -> ok; true ->
        die("Element count must be positive~n")
    end,
    if KeySize > 0 -> ok; true ->
        die("Key size must be positive.~n")
    end,
    if ValSize > 0 -> ok; true ->
        die("Value size must be positive.~n")
    end,

    {ok, Hash} = khash:new(),
    Seq = lists:seq(1, Sz),
    KVs = [{crypto:strong_rand_bytes(KeySize), crypto:strong_rand_bytes(ValSize)} || _I <- Seq],

    {KT1, _} = timer:tc(fun insert_many/2, [Hash, KVs]),
    {MT1, Map} = timer:tc(fun insert_many/2, [#{}, KVs]),

    {KT2, _} = timer:tc(fun get_many/2, [Hash, KVs]),
    {MT2, _} = timer:tc(fun get_many/2, [Map, KVs]),

    {KT3, _} = timer:tc(fun miss_many/2, [Hash, KVs]),
    {MT3, _} = timer:tc(fun miss_many/2, [Map, KVs]),

    io:format("Dataset: ~p elements, key size ~p bytes, value size ~p bytes~n~n",
        [Sz, KeySize, ValSize]),
    io:format("Kazlib hash: ~12.1f inserts/sec~n", [Sz * 1_000_000 / KT1]),
    io:format("Erlang map:  ~12.1f inserts/sec~n~n", [Sz * 1_000_000 / MT1]),
    io:format("Kazlib hash: ~12.1f lookups/sec~n", [Sz * 1_000_000 / KT2]),
    io:format("Erlang map:  ~12.1f lookups/sec~n~n", [Sz * 1_000_000 / MT2]),
    io:format("Kazlib hash: ~12.1f misses/sec~n", [Sz * 1_000_000 / KT3]),
    io:format("Erlang map:  ~12.1f misses/sec~n", [Sz * 1_000_000 / MT3]),
ok;

main(_) ->
    Args = [escript:script_name()],
    die("usage: ~s element_count key_size value_size~n", Args).


to_int(Val) when is_integer(Val) ->
    Val;
to_int(Val) when is_binary(Val) ->
    to_int(binary_to_list(Val));
to_int(Val) when is_list(Val) ->
    try
        list_to_integer(Val)
    catch _:_ ->
        die("Invalid integer: ~w~n", [Val])
    end;
to_int(Val) ->
    die("Invalid integer: ~w~n", [Val]).


die(Message) ->
    die(Message, []).

die(Format, Args) ->
    io:format(Format, Args),
    init:stop().

insert_many(#{} = M0, KVs) ->
    lists:foldl(
        fun({Key, Val}, M) ->
            maps:put(Key, Val, M)
        end, 
        M0, 
        KVs
    );
insert_many(Hash, KVs) ->
    [khash:put(Hash, K, V) || {K, V} <- KVs].

get_many(#{} = M, KVs) ->
    [maps:get(K, M) =:= V || {K, V} <- KVs];
get_many(Hash, KVs) ->
    [khash:get(Hash, K) =:= V || {K, V} <- KVs].

miss_many(#{} = M, KVs) ->
    [maps:get(V, M, nope) =:= nope || {_K, V} <- KVs];
miss_many(Hash, KVs) ->
    [khash:get(Hash, V) =:= not_found || {_K, V} <- KVs].