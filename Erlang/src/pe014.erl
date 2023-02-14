%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% The following iterative sequence is defined for the set of
%%% positive integers:
%%%
%%%   n -> n/2 (n is even)
%%%   n -> 3n + 1 (n is odd)
%%%
%%% Using the rule above and starting with 13, we generate the
%%% following sequence:
%%%
%%%   13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
%%%
%%% It can be seen that this sequence (starting at 13 and
%%% finishing at 1) contains 10 terms. Although it has not
%%% been proved yet (Collatz Problem), it is thought that
%%% all starting numbers finish at 1.
%%%
%%% Which starting number, under one million, produces the
%%% longest chain?
%%%
%%% NOTE: Once the chain starts the terms are allowed to go
%%% above one million.
%%% @end
%%%======================================================================
-module(pe014).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(LIMIT, 1000000).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The starting number, under ~p, with the longest chain is ~p.~n", [?LIMIT, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    {Result, _, _} = tools:forward(1, ?LIMIT, {0, 0, #{}},
        fun (Number, {Previous, Size, Table}) ->
            {NextTable, NextSize} = get_size(Table, Number),
            {FinalSize, FinalNumber} = max({NextSize, Number}, {Size, Previous}),
            {FinalNumber, FinalSize, NextTable}
        end
    ),
    Result.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the size of the sequence chain.
%% @param Table The table to modify.
%% @param Number The number to check.
%% @returns A tuple with the modified table and the size of the chain
%% for the given number.
%% @end
%%-----------------------------------------------------------------------
get_size(Table, Number) ->
    case {maps:is_key(Number, Table), Number} of
        {true, _} ->
            {_, Size} = maps:get(Number, Table),
            {Table, Size};
        {false, N} when N < 1 ->
            {Table, 0};
        {false, 1} ->
            {Table#{ 1 => {1, 1}}, 1};
        {false, _} ->
            NextNumber = case (Number rem 2) =:= 0 of
                             true -> Number div 2;
                             false -> 3 * Number + 1
                         end,
            {NextTable, Size} = get_size(Table, NextNumber),
            {NextTable#{ Number => {NextNumber, Size + 1}}, Size + 1}
    end.
