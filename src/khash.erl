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
    size/1,
    iter/1,
    iter_next/1,
    fold/3
]).


-define(NOT_LOADED, not_loaded(?LINE)).


-type kv() :: {any(), any()}.
-type khash() :: term().
-type khash_iter() :: term().
-type option() :: [].


-spec new() -> {ok, khash()}.
new() ->
    new([]).


-spec new([option()]) -> {ok, khash()}.
new(_Options) ->
    ?NOT_LOADED.


-spec from_list([kv()]) -> {ok, khash()}.
from_list(KVList) ->
    from_list(KVList, []).


-spec from_list([kv()], [option()]) -> {ok, khash()}.
from_list(KVList, Options) ->
    {ok, Hash} = ?MODULE:new(Options),
    lists:foreach(fun({Key, Val}) ->
        ?MODULE:put(Hash, Key, Val)
    end, KVList),
    {ok, Hash}.


-spec to_list(khash()) -> [kv()].
to_list(_Hash) ->
    ?NOT_LOADED.


-spec clear(khash()) -> ok.
clear(_Hash) ->
    ?NOT_LOADED.


-spec lookup(khash(), any()) -> {value, any()} | not_found.
lookup(_Hash, _Key) ->
    ?NOT_LOADED.


-spec get(khash(), any()) -> any().
get(Hash, Key) ->
    get(Hash, Key, undefined).


-spec get(khash(), any(), any()) -> any().
get(_Hash, _Key, _Default) ->
    ?NOT_LOADED.


-spec put(khash(), any(), any()) -> ok.
put(_Hash, _Key, _Value) ->
    ?NOT_LOADED.


-spec del(khash(), any()) -> ok.
del(_Hash, _Key) ->
    ?NOT_LOADED.


-spec size(khash()) -> non_neg_integer().
size(_Hash) ->
    ?NOT_LOADED.


-spec iter(khash()) -> {ok, khash_iter()}.
iter(_Hash) ->
    ?NOT_LOADED.


-spec iter_next(khash_iter()) ->
        kv() | end_of_table | {error, expired_iterator}.
iter_next(_Iter) ->
    ?NOT_LOADED.


-spec fold(khash(), fun(), any()) -> any().
fold(Hash, FoldFun, Acc) ->
    {ok, Iter} = ?MODULE:iter(Hash),
    fold_int(Iter, FoldFun, Acc).


fold_int(Iter, FoldFun, Acc) ->
    case ?MODULE:iter_next(Iter) of
        {Key, Value} ->
            NewAcc = FoldFun(Key, Value, Acc),
            fold_int(Iter, FoldFun, NewAcc);
        end_of_table ->
            Acc
    end.


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
