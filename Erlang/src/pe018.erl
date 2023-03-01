%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% By starting at the top of the triangle below and moving to
%%% adjacent numbers on the row below, the maximum total from top
%%% to bottom is 23.
%%%
%%%        3              3
%%%       7 4            7 .
%%%      2 4 6          . 4 .
%%%     8 5 9 3        . . 9 .
%%%
%%% That is, 3 + 7 + 4 + 9 = 23.
%%%
%%% Find the maximum total from top to bottom of the triangle
%%% below:
%%%
%%%                   75
%%%                  95 64
%%%                 17 47 82
%%%                18 35 87 10
%%%               20 04 82 47 65
%%%              19 01 23 75 03 34
%%%             88 02 77 73 07 63 67
%%%            99 65 04 28 06 16 70 92
%%%           41 41 26 56 83 40 80 70 33
%%%          41 48 72 33 47 32 37 16 94 29
%%%         53 71 44 65 25 43 91 52 97 51 14
%%%        70 11 33 28 77 73 17 78 39 68 17 57
%%%       91 71 52 38 17 14 91 43 58 50 27 29 48
%%%      63 66 04 68 89 53 67 30 73 16 69 87 40 31
%%%     04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
%%%
%%% NOTE: As there are only 16384 routes, it is possible to solve
%%% this problem by trying every route. However, Problem 67, is
%%% the same challenge with a triangle containing one-hundred
%%% rows; it cannot be solved by brute force, and requires a
%%% clever method! ;o)
%%% @end
%%%======================================================================
-module(pe018).
-author("Gorka Suárez García").
-export([main/0, result/0]).
-include("tools.hrl").

-record(node, {sum = -1, path = []}).

-define(WORLD, [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20,  4, 82, 47, 65],
    [19,  1, 23, 75,  3, 34],
    [88,  2, 77, 73,  7, 63, 67],
    [99, 65,  4, 28,  6, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]
]).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    Result = result(),
    io:format("The maximum total from top to bottom of the triangle is ~p.~n", [Result#node.sum]),
    io:format("Final path: ~p~n", [Result#node.path]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    % Check all the rows inside the given world:
    Values = lists:foldl(
        fun (Row, []) ->
                % On the first row of the world, just add to the
                % current objects the initial path object with the
                % root of the world:
                [#node{sum = Value, path = [Value]} || Value <- Row];

            (Row, Previous) ->
                % Check each column of the row:
                lists:map(
                    fun(Index) ->
                        % When the row isn't the first one, we'll select the
                        % left and right path objects from the previous results:
                        Left = tools:if_else(
                            (Index - 1) > 0,
                            ?ACTION(lists:nth(Index - 1, Previous)),
                            #node{}
                        ),
                        Right = tools:if_else(
                            Index =< length(Previous),
                            ?ACTION(lists:nth(Index, Previous)),
                            #node{}
                        ),
                        % Then, we'll select the maximum value between the left
                        % and the right, to create a new path object result and
                        % add it to the current results:
                        Selected = tools:if_else(
                            Left#node.sum >= Right#node.sum,
                            Left,
                            Right
                        ),
                        Value = lists:nth(Index, Row),
                        #node{
                            sum  = Selected#node.sum + Value,
                            path = Selected#node.path ++ [Value]
                        }
                    end,
                    lists:seq(1, length(Row))
                )
        end,
        [],
        ?WORLD
    ),
    % Select the maximum valued path from the previous results:
    tools:max(Values, fun(Value) -> Value#node.sum end).
