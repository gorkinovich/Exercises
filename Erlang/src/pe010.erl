%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%%%
%%% Find the sum of all the primes below two million.
%%% @end
%%%======================================================================
-module(pe010).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(LIMIT, 2000000).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The sum of all the primes below ~p is ~p.~n", [?LIMIT, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    tools:reduce_while(
        primes, 0, ?LIMIT,
        fun (R, X) ->
            %io:format("~p + ~p~n", [R, X]),
            R + X
        end
    ).
