%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% n! means n × (n - 1) × ... × 3 × 2 × 1
%%%
%%% For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
%%% and the sum of the digits in the number 10! is
%%% 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
%%%
%%% Find the sum of the digits in the number 100!
%%% @end
%%%======================================================================
-module(pe020).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(GOAL, 100).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The sum of the digits in the number ~p! is ~p.~n", [?GOAL, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    lists:sum(tools:get_digits(tools:factorial(?GOAL))).
