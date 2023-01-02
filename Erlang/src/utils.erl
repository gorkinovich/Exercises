%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This module contains utility functions.
%%% @end
%%%======================================================================
-module(utils).
-author("Gorka Suárez García").
-export([
    factorial/1, pow/2, product/1, find/2, all/2, any/2,
    cartesian/2, cartesian/3, combinations/2, combinations/3
]).

%%%======================================================================
%%% Macros
%%%======================================================================

-record(loop, {
    items = [],
    step = 0,
    size = 0,
    current = []
}).

%%%======================================================================
%%% Math functions
%%%======================================================================

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
%% Internal function for 'fun factorial/1'.
%% @end
%%-----------------------------------------------------------------------
factorial(Number, Current) when Number =< 1 -> Current;
factorial(Number, Current) -> factorial(Number - 1, Current * Number).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the operation Left to the power of Right.
%% @param Left The left operand.
%% @param Right The right operand.
%% @returns The result of the operation.
%% @end
%%-----------------------------------------------------------------------
pow(Left, Right) when is_integer(Left), is_integer(Right) ->
    trunc(math:pow(Left, Right));
pow(Left, Right) ->
    math:pow(Left, Right).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the product of the elements in a list.
%% @param Numbers The list to check.
%% @returns The product of the elements.
%% @end
%%-----------------------------------------------------------------------
product([]) -> 0;
product(Numbers) -> product(Numbers, 1).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun product/1'.
%% @end
%%-----------------------------------------------------------------------
product([], Current) -> Current;
product([X|XS], Current) -> product(XS, X * Current).

%%%======================================================================
%%% Loop functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Finds a result inside a list after applying a function.This function
%% is design to be lazy in its execution, it will stop its execution if
%% the result is found.
%% @param List The list with the elements to check.
%% @param Function The function to apply.
%% @returns {'ok', Result} if the result is found, otherwise 'nothing'.
%% @end
%%-----------------------------------------------------------------------
find([], _) ->
    nothing;
find([Item | Items], Function) ->
    case Function(Item) of
        {ok, Result} -> {ok, Result};
        _ -> find(Items, Function)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Checks if every element in a list fulfills a predicate. This function
%% is design to be lazy in its execution, it will stop its execution if
%% any element gets 'false' with the predicate.
%% @param List The list with the elements to check.
%% @param Predicate The predicate to check.
%% @returns 'true' if every element fulfills the predicate.
%% @end
%%-----------------------------------------------------------------------
all([], _) ->
    true;
all([Item | Items], Predicate) ->
    case Predicate(Item) of
        false -> false;
        _ -> all(Items, Predicate)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Checks if any element in a list fulfills a predicate. This function
%% is design to be lazy in its execution, it will stop its execution if
%% any element gets 'true' with the predicate.
%% @param List The list with the elements to check.
%% @param Predicate The predicate to check.
%% @returns 'true' if any element fulfills the predicate.
%% @end
%%-----------------------------------------------------------------------
any([], _) ->
    false;
any([Item | Items], Predicate) ->
    case Predicate(Item) of
        true -> true;
        _ -> any(Items, Predicate)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the cartesian product of a list of elements. (NOTE: Please
%% be very careful using this function, avoid big lists and sizes.)
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @returns A list of lists with the cartesian product.
%% @end
%%-----------------------------------------------------------------------
cartesian(List, Size) ->
    lists:reverse(cartesian_loop(List, [#loop{ items = List, size = Size}], [])).

%%-----------------------------------------------------------------------
%% @doc
%% Runs the cartesian product of a list of elements. (NOTE: Please
%% be very careful using this function, avoid big lists and sizes.)
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @param Function The operation for each element.
%% @end
%%-----------------------------------------------------------------------
cartesian(List, Size, Function) ->
    cartesian_loop(List, [#loop{ items = List, size = Size}], Function).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun cartesian/2,3'.
%% @end
%%-----------------------------------------------------------------------
cartesian_loop(_, [], Result) ->
    Result;
cartesian_loop(List, [State | Stack], Result) when State#loop.step >= State#loop.size ->
    cartesian_loop(List, Stack, update_loop_result(lists:reverse(State#loop.current), Result));
cartesian_loop(List, [State | Stack], Result) ->
    case State#loop.items of
        [] ->
            cartesian_loop(List, Stack, Result);
        _ ->
            NextState =  State#loop{
                items = tl(State#loop.items)
            },
            StepState = State#loop{
                items = List,
                current = [hd(State#loop.items) | State#loop.current],
                step = State#loop.step + 1
            },
            cartesian_loop(List, [StepState, NextState | Stack], Result)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the combinations of a list of elements.
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @returns A list of lists with the combinations.
%% @end
%%-----------------------------------------------------------------------
combinations(List, Size) ->
    lists:reverse(combinations_loop([#loop{ items = List, size = Size}], [])).

%%-----------------------------------------------------------------------
%% @doc
%% Runs the combinations of a list of elements.
%% @param List The list with the elements.
%% @param Size The size of elements to combine.
%% @param Function The operation for each element.
%% @end
%%-----------------------------------------------------------------------
combinations(List, Size, Function) ->
    combinations_loop([#loop{ items = List, size = Size}], Function).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun combinations/2,3'.
%% @end
%%-----------------------------------------------------------------------
combinations_loop([], Result) ->
    Result;
combinations_loop([State | Stack], Result) when State#loop.step >= State#loop.size ->
    combinations_loop(Stack, update_loop_result(lists:reverse(State#loop.current), Result));
combinations_loop([State | Stack], Result) ->
    ItemsLength = length(State#loop.items),
    Hole = State#loop.size - State#loop.step,
    FlagEqual = ItemsLength =:= Hole,
    FlagLess = ItemsLength < Hole,
    case {FlagEqual, FlagLess} of
        {_, true} ->
            combinations_loop(Stack, Result);
        {true, _} ->
            Current = lists:reverse(State#loop.current) ++ State#loop.items,
            combinations_loop(Stack, update_loop_result(Current, Result));
        _ ->
            NextItems = tl(State#loop.items),
            NextState = State#loop{
                items = NextItems
            },
            StepState = State#loop{
                items = NextItems,
                current = [hd(State#loop.items) | State#loop.current],
                step = State#loop.step + 1
            },
            combinations_loop([StepState, NextState | Stack], Result)
    end.

%%%======================================================================
%%% Auxiliary functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Updates the result of a loop with a value.
%% @param Value The value to use.
%% @param Result The result to update.
%% @returns The result updated.
%% @end
%%-----------------------------------------------------------------------
update_loop_result(Value, Result) when is_list(Result) ->
    [Value | Result];
update_loop_result(Value, Result) when is_function(Result) ->
    Result(Value),
    Result;
update_loop_result(Value, Result) ->
    case sets:is_set(Result) of
        true -> sets:add_element(Value, Result);
        false -> Result
    end.
