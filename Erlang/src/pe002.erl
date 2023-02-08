%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% Each new term in the Fibonacci sequence is generated by adding
%%% the previous two terms. By starting with 1 and 2, the first 10
%%% terms will be: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
%%%
%%% By considering the terms in the Fibonacci sequence whose
%%% values do not exceed four million, find the sum of the
%%% even-valued terms.
%%% @end
%%%======================================================================
-module(pe002).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(LIMIT, 4000000).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The sum of the even-valued terms below four million is ~p.~n", [result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    tools:reduce_while(
        fibonacci, 0, ?LIMIT,
        fun (A, Current) when (Current rem 2) == 0 -> A + Current;
            (A, _) -> A
        end
    ).
