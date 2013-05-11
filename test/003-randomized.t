#! /usr/bin/env escript
%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-mode(compile).

num_cycles() -> 10000.

main([]) ->
    code:add_pathz("test"),
    random:seed(erlang:now()),

    util:run(num_cycles(), fun() ->
        test_random(),
        ok
    end).


test_random() ->
    D = dict:new(),
    {ok, H} = khash:new(),

    Actions = [
        {0.1, fun(S) -> run_clear(S) end},
        {1.0, fun(S) -> run_get2(S) end},
        {1.0, fun(S) -> run_get3(S) end},
        {1.0, fun(S) -> run_put(S) end},
        {1.0, fun(S) -> run_del(S) end},
        {0.5, fun(S) -> run_size(S) end},
        %{0.3, fun(S) -> run_keys(S) end},
        {0.3, fun(S) -> run_to_list(S) end}
    ],

    run(Actions, num_cycles(), {D, H}).


run(_, N, _S) when N =< 0 ->
    ok;
run(Actions, N, S0) ->
    Action = weighted_choice(Actions),
    S1 = Action(S0),
    true = check_state(S1),
    run(Actions, N-1, S1).


run_clear({_D0, H}) ->
    ok = khash:clear(H),
    {dict:new(), H}.


run_get2({D, H}) ->
    K = random_key(D),
    case dict:find(K, D) of
        {ok, Value} ->
            {value, Value} = khash:lookup(H, K),
            Value = khash:get(H, K);
        error ->
            not_found = khash:lookup(H, K),
            undefined = khash:get(H, K)
    end,
    {D, H}.


run_get3({D, H}) ->
    K = random_key(D),
    case dict:find(K, D) of
        {ok, Value} ->
            {value, Value} = khash:lookup(H, K),
            Value = khash:get(H, K);
        error ->
            Val = random_val(),
            Val = khash:get(H, K, Val)
    end,
    {D, H}.


run_put({D0, H}) ->
    K = random_key(D0),
    V = random_val(),
    D1 = dict:store(K, V, D0),
    ok = khash:put(H, K, V),
    {D1, H}.


run_del({D0, H}) ->
    K = random_key(D0),
    D1 = case dict:is_key(K, D0) of
        true ->
            ok = khash:del(H, K),
            dict:erase(K, D0);
        false ->
            not_found = khash:del(H, K),
            D0
    end,
    {D1, H}.


run_size({D, H}) ->
    S = dict:size(D),
    S = khash:size(H),
    {D, H}.


%run_keys({D, H}) ->
%    DKeys = lists:sort(dict:fetch_keys(D)),
%    {ok, HKeys0} = khash:keys(H),
%    HKeys = lists:sort(HKeys0),
%    DKeys = HKeys,
%    {D, H}.


run_to_list({D, H}) ->
    DKVs = lists:sort(dict:to_list(D)),
    HKVs = lists:sort(khash:to_list(H)),
    DKVs = HKVs,
    {D, H}.


check_state({D, H}) ->
    DKVs = lists:sort(dict:to_list(D)),
    HKVs = lists:sort(khash:to_list(H)),
    etap:is(DKVs, HKVs, "State matches dict implementation").


weighted_choice(Items0) ->
    Items = lists:sort(Items0),
    Sum = lists:sum([W || {W, _} <- Items]),
    Choice = random:uniform() * Sum,
    weighted_choice(Items, 0.0, Choice).


weighted_choice([], _, _) ->
    throw(bad_choice);
weighted_choice([{W, _} | Rest], S, C) when W + S < C ->
    weighted_choice(Rest, W+S, C);
weighted_choice([{_, I} | _], _, _) ->
    I.


random_key(D) ->
    Keys = lists:usort(dict:fetch_keys(D) ++ [foo]),
    lists:nth(random:uniform(length(Keys)), Keys).


random_val() ->
    gen_term:any().
