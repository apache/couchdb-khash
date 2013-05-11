%% This file is part of khash released under the MIT license.
%% See the LICENSE file for more information.
%% Copyright 2013 Cloudant, Inc <support@cloudant.com>

-module(util).
-compile(export_all).


init_code_path() ->
    code:add_pathz("ebin").


run(Plan, Fun) ->
    init_code_path(),
    etap:plan(Plan),
    case (catch Fun()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally:~n~p", [Other])),
            timer:sleep(500),
            etap:bail(Other)
    end,
    ok.

