#! /usr/bin/env escript
%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-mode(compile).

num_cycles() -> 10000.

main([]) ->
    code:add_pathz("test"),
    util:run(12, fun() ->
        test_basic(),
        ok
    end).


test_basic() ->
    {ok, C} = khash:new(),
    etap:is(khash:lookup(C, <<"foo">>), not_found, "Lookup missing is ok"),
    etap:is(khash:get(C, <<"foo">>), undefined, "Get missing is ok"),
    etap:is(khash:del(C, <<"foo">>), not_found, "Del missing is ok"),
    etap:is(khash:put(C, <<"foo">>, bar), ok, "Stored a key"),
    etap:is(khash:lookup(C, <<"foo">>), {value, bar}, "Lookuped a key"),
    etap:is(khash:get(C, <<"foo">>), bar, "Retrieved a key"),
    etap:is(khash:put(C, <<"bar">>, foo), ok, "Stored a key"),
    etap:is(khash:size(C), 2, "Correct size for hash"),
    etap:is(khash:del(C, <<"foo">>), ok, "Deleted a key"),
    etap:is(khash:size(C), 1, "Correct size after delete"),
    etap:is(khash:clear(C), ok, "Cleared the hash"),
    etap:is(khash:size(C), 0, "Correct size after clear").
