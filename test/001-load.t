#! /usr/bin/env escript
%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    Modules = [
        khash
    ],

    etap:plan(length(Modules)),
    lists:foreach(fun(M) ->
        etap:loaded_ok(M, "Loaded " ++ atom_to_list(M))
    end, Modules),
    etap:end_tests().

