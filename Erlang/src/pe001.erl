%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% If we list all the natural numbers below 10 that are multiples of
%%% 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
%%%
%%% Find the sum of all the multiples of 3 or 5 below 1000.
%%% @end
%%%======================================================================
-module(pe001).
-author("Gorka Suárez García").
-export([main/0]).

-define(CANDIDATE, 1000).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    Result = lists:sum([
        Number || Number <- lists:seq(1, ?CANDIDATE - 1),
        is_multiple(Number, 3) orelse is_multiple(Number, 5)
    ]),
    io:format("The sum of all the multiples of 3 or 5 below 1000 is ~p.~n", [Result]).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Checks if the LHS is a multiple of the RHS.
%% @param LHS The left-hand side.
%% @param RHS The right-hand side.
%% @returns 'true' when the LHS is a multiple of the RHS.
%% @end
%%-----------------------------------------------------------------------
is_multiple(LHS, RHS) ->
    (LHS rem RHS) == 0.
