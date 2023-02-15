%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% Starting in the top left corner of a 2×2 grid, there are 6
%%% routes (without backtracking) to the bottom right corner.
%%%
%%%     1 2 3    1 2 .    1 2 .
%%%     . . 4    . 3 4    . 3 .
%%%     . . 5    . . 5    . 4 5
%%%
%%%     1 . .    1 . .    1 . .
%%%     2 3 4    2 3 .    2 . .
%%%     . . 5    . 4 5    3 4 5
%%%
%%% How many routes are there through a 20×20 grid?
%%% @end
%%%======================================================================
-module(pe015).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(SIZE, 20).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The number of routes in a ~px~p grid are ~p.~n", [?SIZE, ?SIZE, result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    {Result, _} = get_pascal_triangle(#{}, ?SIZE * 2, ?SIZE),
    Result.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the (n, k) number inside the pascal triangle.
%% @param Table The table with the previous values.
%% @param N The row in the triangle.
%% @param K The column in the triangle.
%% @returns The natural number for (n, k).
%% @end
%%-----------------------------------------------------------------------
get_pascal_triangle(_, N, K) when N < 0; K < 0 ->
    throw(value_error);
get_pascal_triangle(Table, 0, _) ->
    {1, Table};
get_pascal_triangle(Table, N, K) when 0 < K, K < N ->
    case maps:is_key({N, K}, Table) of
        true ->
            {maps:get({N, K}, Table), Table};
        false ->
            {Left, LeftTable} = get_pascal_triangle(Table, N - 1, K - 1),
            {Right, RightTable} = get_pascal_triangle(LeftTable, N - 1, K),
            {Left + Right, RightTable#{ {N, K} => Left + Right }}
    end;
get_pascal_triangle(Table, _, _) ->
    {1, Table}.
