#! /usr/bin/env escript
%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-mode(compile).

main([]) ->
    code:add_pathz("test"),
    util:run(12, fun() ->
        test(),
        ok
    end).


test() ->
    test_basic(),
    test_multi(),
    test_expiration(),
    test_no_expiration(),
    ok.


test_basic() ->
    {ok, H} = khash:new(),
    khash:put(H, foo, bar),
    {ok, I} = khash:iter(H),
    etap:is(khash:iter_next(I), {foo,bar}, "Got only kv pair as first element"),
    etap:is(khash:iter_next(I), end_of_table, "Only the one kv pair exists"),
    FoldFun = fun(K, V, Acc) -> [{K,V} | Acc] end,
    etap:is(khash:fold(H, FoldFun, []), [{foo,bar}], "fold works").


test_multi() ->
    {ok, H} = khash:new(),
    KVs = [{I, I} || I <- lists:seq(1, 10)],
    lists:foreach(fun({K,V}) -> khash:put(H, K, V) end, KVs),
    {ok, I} = khash:iter(H),
    ReadKVs = test_multi_read(I, []),
    etap:is(ReadKVs, KVs, "Read the same exact key/val pairs").


test_multi_read(Iter, KVs) ->
    case khash:iter_next(Iter) of
        {K, V} ->
            test_multi_read(Iter, [{K,V} | KVs]);
        end_of_table ->
            lists:sort(KVs)
    end.


test_expiration() ->
    test_expiration("put", fun(H) ->
        khash:put(H, foo, bar2),
        ok
    end),

    test_expiration("del", fun(H) ->
        khash:del(H, foo),
        ok
    end),

    test_expiration("clear", fun(H) ->
        khash:clear(H),
        ok
    end).


test_expiration(FunName, Fun) ->
    Error = {error, expired_iterator},
    {ok, H} = khash:new(),
    khash:put(H, foo, bar),
    {ok, I} = khash:iter(H),
    ok = Fun(H),
    Msg = FunName ++ " expires iterators",
    etap:is(khash:iter_next(I), Error, Msg).


test_no_expiration() ->
    test_no_expiration("to_list", fun(H) ->
        [{foo,bar}] = khash:to_list(H),
        ok
    end),

    test_no_expiration("lookup", fun(H) ->
        {value,bar} = khash:lookup(H,foo),
        ok
    end),

    test_no_expiration("get", fun(H) ->
        bar = khash:get(H, foo),
        ok
    end),

    test_no_expiration("size", fun(H) ->
        1 = khash:size(H),
        ok
    end),

    test_no_expiration("iteration", fun(H) ->
        {ok, I} = khash:iter(H),
        {foo, bar} = khash:iter_next(I),
        end_of_table = khash:iter_next(I),
        ok
    end).


test_no_expiration(FunName, Fun) ->
    Error = {error, expired_iterator},
    {ok, H} = khash:new(),
    khash:put(H, foo, bar),
    {ok, I} = khash:iter(H),
    ok = Fun(H),
    Msg = FunName ++ " doesn't expire iterators",
    etap:isnt(khash:iter_next(I), Error, Msg).
