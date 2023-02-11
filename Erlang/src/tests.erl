%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This module contains tests functions.
%%% @end
%%%======================================================================
-module(tests).
-author("Gorka Suárez García").
-include_lib("eunit/include/eunit.hrl").

-define(TEST(Desc, Func), {setup, fun setup/0, fun cleanup/1, {Desc, Func}}).

setup() -> ok.
cleanup(_) -> ok.

main_test_() ->
    [
        ?TEST("Problem 001", ?_assertMatch(233168, pe001:result())),
        ?TEST("Problem 002", ?_assertMatch(4613732, pe002:result())),
        ?TEST("Problem 003", ?_assertMatch(6857, pe003:result())),
        ?TEST("Problem 004", ?_assertMatch({906609, 913, 993}, pe004:result())),
        ?TEST("Problem 005", ?_assertMatch(232792560, pe005:result())),
        ?TEST("Problem 006", ?_assertMatch(25164150, pe006:result())),
        ?TEST("Problem 007", ?_assertMatch(104743, pe007:result())),
        ?TEST("Problem 008", ?_assertMatch(23514624000, pe008:result())),
        ?TEST("Problem 009", ?_assertMatch({200, 375, 425}, pe009:result())),
        ?TEST("Problem 010", ?_assertMatch(142913828922, pe010:result())),
        ?TEST("Problem 011", ?_assertMatch(70600674, pe011:result())),
        ?TEST("Problem 012", ?_assertMatch(76576500, pe012:result()))
    ].
