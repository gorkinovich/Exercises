%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% This module contains utility functions.
%%% @end
%%%======================================================================
-module(mathutils).
-author("Gorka Suárez García").
-export([factorial/1, combinations/2, cartesian/2, cartesian/3]).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the factorial of a natural number.
%% @param Number The number to check.
%% @returns The factorial of the given number.
%% @end
%%-----------------------------------------------------------------------
factorial(Number) -> factorial(Number, 1).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the factorial of a natural number.
%% @param Number The number to check.
%% @param Accum The accumulated value to return.
%% @returns The factorial of the given number.
%% @end
%%-----------------------------------------------------------------------
factorial(Number, Accum) when Number =< 1 -> Accum;
factorial(Number, Accum) -> factorial(Number - 1, Accum * Number).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the combinations of a list of elements.
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @returns A list of lists with the combinations.
%% @end
%%-----------------------------------------------------------------------
combinations(List, Size) ->
    singleton:start_link(),
    Variable = {?MODULE, combinations, make_ref()},
    singleton:set(Variable, sets:new()),
    cartesian(
        List, Size,
        fun(Current) ->
            case Size =:= length(lists:uniq(Current)) of
                false ->
                    ok;
                true ->
                    singleton:apply(
                        Variable,
                        fun(Value) ->
                            sets:add_element(lists:sort(Current), Value)
                        end
                    )
            end
        end
    ),
    lists:sort(sets:to_list(singleton:get(Variable))).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the cartesian product of a list of elements.
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @returns A list of lists with the cartesian product.
%% @end
%%-----------------------------------------------------------------------
cartesian(List, Size) ->
    singleton:start_link(),
    Variable = {?MODULE, cartesian, make_ref()},
    singleton:set(Variable, []),
    cartesian(
        List, Size,
        fun(Current) ->
            singleton:apply(
                Variable,
                fun(Value) ->
                    [Current | Value]
                end
            )
        end
    ),
    lists:reverse(singleton:get(Variable)).

%%-----------------------------------------------------------------------
%% @doc
%% Runs the cartesian product of a list of elements.
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @param Function The operation for each element of the product.
%% @end
%%-----------------------------------------------------------------------
cartesian(List, Size, Function) ->
    cartesian_loop(List, Size, Function, 0, []).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun cartesian/2'.
%% @end
%%-----------------------------------------------------------------------
cartesian_loop(_, Size, Function, Step, Current) when Step >= Size ->
    Function(lists:reverse(Current));
cartesian_loop(List, Size, Function, Step, Current) ->
    lists:foldl(
        fun(Item, _) ->
            cartesian_loop(List, Size, Function, Step + 1, [Item | Current])
        end,
        ok,
        List
    ).
