%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% The prime factors of 13195 are 5, 7, 13 and 29.
%%%
%%% What is the largest prime factor of the number 600851475143?
%%% @end
%%%======================================================================
-module(pe003).
-author("Gorka Suárez García").
-export([main/0]).

-define(CANDIDATE, 600851475143).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    Factors = get_prime_factors(?CANDIDATE),
    Result = hd(lists:reverse(Factors)),
    io:format("The largest prime factor of ~p is ~p.~n", [?CANDIDATE, Result]).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the prime factors of a number.
%% @param Number The number to check.
%% @returns A list with the prime factors.
%% @end
%%-----------------------------------------------------------------------
get_prime_factors(Number) ->
    get_prime_factors(Number, 2, []).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the prime factors of a number.
%% @param Number The number to check.
%% @param Divisor The current divisor to check.
%% @param Factors The accumulated list of prime factors.
%% @returns A list with the prime factors.
%% @end
%%-----------------------------------------------------------------------
get_prime_factors(Number, Divisor, Factors) when Number < Divisor ->
    lists:reverse(Factors);
get_prime_factors(Number, Divisor, Factors) ->
    NextDivisor = next_divisor(Divisor),
    case reduce_division(Number, Divisor) of
        Number ->
            get_prime_factors(Number, NextDivisor, Factors);
        NextNumber ->
            get_prime_factors(NextNumber, NextDivisor, [Divisor | Factors])
    end.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the next divisor for the prime factors.
%% @param Divisor The divisor to check.
%% @returns The next divisor number.
%% @end
%%-----------------------------------------------------------------------
next_divisor(2) -> 3;
next_divisor(Divisor) -> Divisor + 2.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Reduce a number with a division multiple times.
%% @param Number The number to reduce.
%% @param Divisor The divisor number.
%% @returns The reduced number.
%% @end
%%-----------------------------------------------------------------------
reduce_division(Number, Divisor) when (Number rem Divisor) =:= 0 ->
    reduce_division(Number div Divisor, Divisor);
reduce_division(Number, _) ->
    Number.
