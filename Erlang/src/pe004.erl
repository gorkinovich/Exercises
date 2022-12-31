%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% A palindromic number reads the same both ways. The largest
%%% palindrome made from the product of two 2-digit numbers is
%%% 9009 = 91 × 99.
%%%
%%% Find the largest palindrome made from the product of two
%%% 3-digit numbers.
%%% @end
%%%======================================================================
-module(pe004).
-author("Gorka Suárez García").
-export([main/0]).

-define(START, 100).
-define(LIMIT, 999).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    {R, N, M} = find_palindrome(?START, ?LIMIT),
    io:format("The largest palindrome made from the product of two"),
    io:format(" 3-digit numbers is ~p * ~p = ~p.~n", [N, M, R]).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Finds the largest palindrome number from the product of two
%% numbers inside a range of numbers.
%% @param Start The start number of the range.
%% @param End The limit number of the range.
%% @returns A triplet with the palindrome and the two numbers of
%% the multiplication if there is a palindrome; otherwise None.
%% @end
%%-----------------------------------------------------------------------
find_palindrome(Start, End) ->
    Candidates = lists:seq(Start, End),
    Palindromes = [{(N * M), N, M} || N <- Candidates, M <- Candidates, is_palindrome(N * M)],
    Sorted = lists:sort(fun({A, _, _}, {B, _, _}) -> A >= B end, Palindromes),
    hd(Sorted).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a value is a palindrome string.
%% @param Victim The value to check.
%% @returns 'true' if the value is a palindrome.
%% @end
%%-----------------------------------------------------------------------
is_palindrome(Victim) when is_integer(Victim) ->
    is_palindrome(integer_to_list(Victim));
is_palindrome(Victim) when is_list(Victim) ->
    Victim =:= lists:reverse(Victim).
