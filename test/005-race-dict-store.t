#! /usr/bin/env escript
%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-mode(compile).

num_cycles() -> 1000000.
num_kvs() -> 10000.

main([]) ->
    % Let the VM settle for a bit
    receive after 1000 -> ok end,

    code:add_pathz("test"),
    util:run(1, fun() ->
        test(),
        ok
    end).


test() ->
    {DTime, _} = timer:tc(fun() -> test_dict() end, []),
    {KTime, _} = timer:tc(fun() -> test_khash() end, []),
    etap:diag("Dict:  ~10b", [DTime]),
    etap:diag("KHash: ~10b", [KTime]),
    etap:is_greater(DTime, KTime, "Dict is slower than khash").


test_dict() ->
    D = dict:from_list(kv_data()),
    test_dict(D, num_cycles()).


test_dict(_D, 0) ->
    ok;
test_dict(D, NumCycles) ->
    D2 = dict:store(1, bing, D),
    test_dict(D2, NumCycles - 1).


test_khash() ->
    {ok, H} = khash:from_list(kv_data()),
    test_khash(H, num_cycles()).


test_khash(_H, 0) ->
    ok;
test_khash(H, NumCycles) ->
    khash:put(H, 1, bing),
    test_khash(H, NumCycles - 1).


kv_data() ->
    [{I, bing} || I <- lists:seq(1, num_kvs())].

