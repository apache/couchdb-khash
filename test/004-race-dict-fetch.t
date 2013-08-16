#! /usr/bin/env escript
%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-mode(compile).

num_cycles() -> 10000000.
num_kvs() -> 5000.


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
    erlang:garbage_collect(),
    D = dict:from_list(kv_data()),
    test_dict(D, num_cycles()).


test_dict(_D, 0) ->
    ok;
test_dict(D, NumCycles) ->
    bing = dict:fetch(1, D),
    test_dict(D, NumCycles - 1).


test_khash() ->
    erlang:garbage_collect(),
    {ok, H} = khash:from_list(kv_data()),
    test_khash(H, num_cycles()).


test_khash(_H, 0) ->
    ok;
test_khash(H, NumCycles) ->
    bing = khash:get(H, 1),
    test_khash(H, NumCycles - 1).


kv_data() ->
    [{I, bing} || I <- lists:seq(1, num_kvs())].

