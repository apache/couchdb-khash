%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-module(khash).
-on_load(init/0).


-export([
    new/0,
    new/1,
    from_list/1,
    from_list/2,
    to_list/1,
    clear/1,
    lookup/2,
    get/2,
    get/3,
    put/3,
    del/2,
    size/1
]).


-define(NOT_LOADED, not_loaded(?LINE)).


-type kv() :: {any(), any()}.
-type hash() :: term().
-type option() :: [shared].


-spec new() -> {ok, hash()}.
new() ->
    new([]).


-spec new([option()]) -> {ok, hash()}.
new(_Options) ->
    ?NOT_LOADED.


-spec from_list([kv()]) -> {ok, hash()}.
from_list(KVList) ->
    from_list(KVList, []).


-spec from_list([kv()], [option()]) -> {ok, hash()}.
from_list(KVList, Options) ->
    {ok, Hash} = ?MODULE:new(Options),
    lists:foreach(fun({Key, Val}) ->
        ?MODULE:put(Hash, Key, Val)
    end, KVList),
    {ok, Hash}.


-spec to_list(hash()) -> [kv()].
to_list(_Hash) ->
    ?NOT_LOADED.


-spec clear(hash()) -> ok.
clear(_Hash) ->
    ?NOT_LOADED.


-spec lookup(hash(), any()) -> {value, any()} | not_found.
lookup(_Hash, _Key) ->
    ?NOT_LOADED.


-spec get(hash(), any()) -> any().
get(Hash, Key) ->
    get(Hash, Key, undefined).


-spec get(hash(), any(), any()) -> any().
get(_Hash, _Key, _Default) ->
    ?NOT_LOADED.


-spec put(hash(), any(), any()) -> ok.
put(_Hash, _Key, _Value) ->
    ?NOT_LOADED.


-spec del(hash(), any()) -> ok.
del(_Hash, _Key) ->
    ?NOT_LOADED.


-spec size(hash()) -> non_neg_integer().
size(_Hash) ->
    ?NOT_LOADED.


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "khash"), 0).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
