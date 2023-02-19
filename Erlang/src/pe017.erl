%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% If the numbers 1 to 5 are written out in words: one, two,
%%% three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
%%% letters used in total.
%%%
%%% If all the numbers from 1 to 1000 (one thousand) inclusive
%%% were written out in words, how many letters would be used?
%%%
%%% NOTE: Do not count spaces or hyphens. For example, 342 (three
%%% hundred and forty-two) contains 23 letters and 115 (one
%%% hundred and fifteen) contains 20 letters. The use of "and"
%%% when writing out numbers is in compliance with British usage.
%%% @end
%%%======================================================================
-module(pe017).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(BEGIN, 1).
-define(FINAL, 1000).

-define(NUMBER_TEN, 10).
-define(NUMBER_TWENTY, 20).
-define(NUMBER_HUNDRED, 100).
-define(NUMBER_THOUSAND, 1000).
-define(NUMBER_MILLION, 1000000).
-define(NUMBER_BILLION, 1000000000).
-define(NUMBER_LIMIT, 1000000000000).

-define(AND, "and").
-define(HUNDRED, "hundred").
-define(THOUSAND, "thousand").
-define(MILLION, "million").
-define(BILLION, "billion").

-define(WORDS_TENS, [
    "", "", "twenty", "thirty", "forty", "fifty",
    "sixty", "seventy", "eighty", "ninety"
]).
-define(WORDS_UNITS, [
    "zero", "one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
]).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("Number of letters of the written numbers from ~p to ~p is ~p.~n",
        [?BEGIN, ?FINAL, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    lists:sum([
        lists:sum([1 || C <- num_to_word(Number), tools:is_alphabetic(C)])
        || Number <- lists:seq(?BEGIN, ?FINAL)
    ]).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Converts a number from digits to words.
%% @param Number The number to convert.
%% @returns The number in words.
%% @end
%%-----------------------------------------------------------------------
num_to_word(Number) when Number >= ?NUMBER_LIMIT; Number < 0 ->
    throw(value_error);
num_to_word(Number) when Number >= ?NUMBER_BILLION ->
    num_to_word(Number, ?NUMBER_BILLION, fun num_to_word/1, [?BILLION]);
num_to_word(Number) when Number >= ?NUMBER_MILLION ->
    num_to_word(Number, ?NUMBER_MILLION, fun num_to_word/1, [?MILLION]);
num_to_word(Number) when Number >= ?NUMBER_THOUSAND ->
    num_to_word(Number, ?NUMBER_THOUSAND, fun num_to_word/1, [?THOUSAND]);
num_to_word(Number) when Number >= ?NUMBER_HUNDRED ->
    LeftToWord = fun(Left) -> lists:nth(Left + 1, ?WORDS_UNITS) end,
    num_to_word(Number, ?NUMBER_HUNDRED, LeftToWord, [?HUNDRED, ?AND]);
num_to_word(Number) when Number >= ?NUMBER_TWENTY ->
    LeftToWord = fun(Left) -> lists:nth(Left + 1, ?WORDS_TENS) end,
    num_to_word(Number, ?NUMBER_TEN, LeftToWord, []);
num_to_word(Number) ->
    lists:nth(Number + 1, ?WORDS_UNITS).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Converts a number from digits to words.
%% @param Number The number to convert.
%% @param Divisor The divisor limit.
%% @param LeftToWord The left side converter.
%% @param MiddleWords The middle words.
%% @returns The number in words.
%% @end
%%-----------------------------------------------------------------------
num_to_word(Number, Divisor, LeftToWord, MiddleWords) ->
    Left = Number div Divisor,
    Right = Number rem Divisor,
    LeftWord = LeftToWord(Left),
    RightWord = tools:if_else(Right =:= 0, "", num_to_word(Right)),
    case {RightWord, MiddleWords} of
        {[], []} -> LeftWord;
        {[], _} -> LeftWord ++ " " ++ hd(MiddleWords);
        _ -> tools:join([LeftWord] ++ MiddleWords ++ [RightWord], " ")
    end.
