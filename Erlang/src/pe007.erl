%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% By listing the first six prime numbers: 2, 3, 5, 7, 11, and
%%% 13, we can see that the 6th prime is 13.
%%%
%%% What is the 10,001st prime number?
%%% @end
%%%======================================================================
-module(pe007).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(CANDIDATE, 10001).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The 10,001st prime number is ~p.~n", [result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    primes:start_link(),
    primes:get(?CANDIDATE - 1).
