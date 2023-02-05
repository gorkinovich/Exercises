%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% The sum of the squares of the first ten natural numbers is,
%%% 1^2 + 2^2 + ... + 10^2 = 385
%%%
%%% The square of the sum of the first ten natural numbers is,
%%% (1 + 2 + ... + 10)^2 = 552 = 3025
%%%
%%% Hence, the difference between the sum of the squares of
%%% the first ten natural numbers and the square of the sum
%%% is 3025 - 385 = 2640.
%%%
%%% Find the difference between the sum of the squares of the
%%% first one hundred natural numbers and the square of the sum.
%%% @end
%%%======================================================================
-module(pe006).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(FIRST, 1).
-define(LAST, 100).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The difference between the sum of the squares and the "
              "square of the sum of the first ~p numbers is ~p.~n",
              [?LAST, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    calc_number(?FIRST, ?LAST).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Calcs the difference between the sum of the squares
%% and the square of the sum of a range of numbers.
%% @param First The first value of the range.
%% @param Last The last value of the range.
%% @returns The difference between the sums.
%% @end
%%-----------------------------------------------------------------------
calc_number(First, Last) ->
    Numbers = lists:seq(First, Last),
    SumSquare = lists:sum([tools:pow(Number, 2) || Number <- Numbers]),
    SquareSum = tools:pow(lists:sum(Numbers), 2),
    SquareSum - SumSquare.
