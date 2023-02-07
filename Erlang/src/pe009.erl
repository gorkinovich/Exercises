%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% A Pythagorean triplet is a set of three natural numbers,
%%% a < b < c, for which: a^2 + b^2 = c^2
%%%
%%% For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
%%%
%%% There exists exactly one Pythagorean triplet for which
%%% a + b + c = 1000.
%%%
%%% Find the product abc.
%%% @end
%%%======================================================================
-module(pe009).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(CANDIDATE, 1000).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    case result() of
        {A, B, C} ->
            io:format("The Pythagorean triplet is (~p, ~p, ~p).~n", [A, B, C]),
            io:format("~p + ~p + ~p = ~p~n", [A, B, C, A + B + C]),
            io:format("~p x ~p x ~p = ~p~n", [A, B, C, A * B * C]);
        _ ->
            io:format("No result was found...~n")
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    case find_triplet(?CANDIDATE) of
        {return, Value} -> Value;
        _ -> nothing
    end.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Finds a pythagorean triplet for which sum is equal to a given
%% candidate number.
%% @param Candidate The candidate number.
%% @returns A triplet with the numbers or 'nothing'.
%% @end
%%-----------------------------------------------------------------------
find_triplet(Candidate) ->
    Condition = fun(A, B, C) ->
        ((A + B + C) == Candidate) andalso (0 < A) andalso (A < B)
            andalso (B < C) andalso (A * A + B * B =:= C * C)
    end,
    tools:forward(floor(Candidate / 3), Candidate,
        fun(C) ->
            tools:forward(1, C,
                fun(B) ->
                    A = Candidate - C - B,
                    case Condition(A, B, C) of
                        true -> {return, {A, B, C}};
                        _ -> ok
                    end
                end
            )
        end
    ).
