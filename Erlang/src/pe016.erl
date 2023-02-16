%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
%%%
%%% What is the sum of the digits of the number 2^1000?
%%% @end
%%%======================================================================
-module(pe016).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(GOAL, 1000).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The sum of the digits of the number 2^~p is ~p.~n", [?GOAL, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    lists:sum(tools:get_digits(tools:pow(2, ?GOAL))).
