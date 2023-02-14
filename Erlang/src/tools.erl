%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This module contains utility functions.
%%% @end
%%%======================================================================
-module(tools).
-author("Gorka Suárez García").
-export([
    % General functions:
    identity/1,

    % Iterator functions:
    find_first/2, reduce_while/4, take_while/2, take_while/3,

    % List functions:
    all_while/3, any_while/3, contains/2, find/2, get_digits/1,
    get_integer/1, get_integers/1, select/2, select_with_string/2,

    % Loop functions:
    forward/3, forward/4,

    % Math functions:
    factorial/1, pow/2, product/1,

    % Mixture functions
    cartesian/2, cartesian/3,
    combinations/2, combinations/3
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
%%% General functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% The identity function.
%% @param Value The value to return.
%% @returns The given value.
%% @end
%%-----------------------------------------------------------------------
identity(Value) -> Value.

%%%======================================================================
%%% Iterator functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Finds the first element that makes true a predicate selector.
%% @param Module The module or iterator of the sequence.
%% @param Predicate The predicate selector to check.
%% @returns The founded value of the sequence.
%% @end
%%-----------------------------------------------------------------------
find_first({Module, _} = Iterator, Predicate) ->
    {Current, NextIterator} = Module:next(Iterator),
    case check_predicate(Current, Predicate) of
        false -> find_first(NextIterator, Predicate);
        _ -> Current
    end;
find_first(Module, Predicate) ->
    Module:start_link(),
    Iterator = Module:iterator(),
    find_first(Iterator, Predicate).

%%-----------------------------------------------------------------------
%% @doc
%% Reduces the elements form an iterator while a predicate selector
%% applies, using a function to aggregate the elements with the result.
%% @param Module The module or iterator of the sequence.
%% @param Initial The initial value of the reduce.
%% @param Selector The predicate selector to check.
%% @param Reduce The aggregate function to apply.
%% @returns The taken values of the sequence.
%% @end
%%-----------------------------------------------------------------------
reduce_while({Module, _} = Iterator, Initial, Selector, Reduce) ->
    {Current, NextIterator} = Module:next(Iterator),
    case check_predicate(Current, Selector) of
        true -> reduce_while(NextIterator, Reduce(Initial, Current), Selector, Reduce);
        _ -> Initial
    end;
reduce_while(Module, Initial, Selector, Reduce) ->
    Module:start_link(),
    Iterator = Module:iterator(),
    reduce_while(Iterator, Initial, Selector, Reduce).

%%-----------------------------------------------------------------------
%% @doc
%% Takes elements form an iterator while a predicate selector applies.
%% @param Module The module or iterator of the sequence.
%% @param Selector The predicate selector to check.
%% @returns The taken values of the sequence.
%% @end
%%-----------------------------------------------------------------------
take_while(Module, Selector) ->
    take_while(Module, normal, Selector, []).

%%-----------------------------------------------------------------------
%% @doc
%% Takes elements form an iterator while a predicate selector applies.
%% @param Module The module or iterator of the sequence.
%% @param State The state of the iteration.
%% @param OnStep The on step event of the iteration.
%% @param Predicate The predicate selector to check.
%% @returns The taken values of the sequence.
%% @end
%%-----------------------------------------------------------------------
take_while(Module, State, OnStep) ->
    take_while(Module, {state, State}, OnStep, []).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun take_while/2'.
%% @end
%%-----------------------------------------------------------------------
take_while({Module, _} = Iterator, {state, State}, OnStep, Result) ->
    {Current, NextIterator} = Module:next(Iterator),
    case OnStep(State, Current) of
        {true, NextState, FinalValue} ->
            take_while(NextIterator, {state, NextState}, OnStep, [FinalValue | Result]);
        _ ->
            lists:reverse(Result)
    end;
take_while({Module, _} = Iterator, normal, Selector, Result) ->
    {Current, NextIterator} = Module:next(Iterator),
    case check_predicate(Current, Selector) of
        true -> take_while(NextIterator, normal, Selector, [Current | Result]);
        _ -> lists:reverse(Result)
    end;
take_while(Module, Mode, Function, Result) ->
    Module:start_link(),
    Iterator = Module:iterator(),
    take_while(Iterator, Mode, Function, Result).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a predicate function is true or false.
%% @param Current The value to check.
%% @param Predicate The predicate function or limit number.
%% @returns 'true' or 'false'.
%% @end
%%-----------------------------------------------------------------------
check_predicate(Current, Predicate) when is_function(Predicate) ->
    Predicate(Current);
check_predicate(Current, Limit) when is_number(Limit) ->
    Current < Limit;
check_predicate(Current, {'<', Limit}) when is_number(Limit) ->
    Current < Limit;
check_predicate(Current, {'<=', Limit}) when is_number(Limit) ->
    Current =< Limit;
check_predicate(Current, {'>', Limit}) when is_number(Limit) ->
    Current > Limit;
check_predicate(Current, {'>=', Limit}) when is_number(Limit) ->
    Current >= Limit;
check_predicate(Current, {Transform, Selector}) when is_function(Transform) ->
    check_predicate(Transform(Current), Selector);
check_predicate(_, _) ->
    false.

%%%======================================================================
%%% List functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Checks if a predicate is true for all the values inside a list
%% while a predicate selector applies.
%% @param List The list to check.
%% @param Predicate The predicate function to check.
%% @param Selector The predicate selector to check.
%% @returns 'true' or 'false'.
%% @end
%%-----------------------------------------------------------------------
all_while([], _, _) ->
    true;
all_while([X | XS], Selector, Predicate) ->
    case check_predicate(X, Selector) of
        true ->
            case Predicate(X) of
                false -> false;
                true -> all_while(XS, Selector, Predicate)
            end;
        _ ->
            true
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Checks if a predicate is true for any value inside a list
%% while a predicate selector applies.
%% @param List The list to check.
%% @param Predicate The predicate function to check.
%% @param Selector The predicate selector to check.
%% @returns 'true' or 'false'.
%% @end
%%-----------------------------------------------------------------------
any_while([], _, _) ->
    false;
any_while([X | XS], Selector, Predicate) ->
    case check_predicate(X, Selector) of
        true ->
            case Predicate(X) of
                true -> true;
                false -> any_while(XS, Selector, Predicate)
            end;
        _ ->
            false
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Checks if a list contains a value.
%% @param List The list to check.
%% @param Value The value to find.
%% @returns 'true' or 'false'.
%% @end
%%-----------------------------------------------------------------------
contains(List, Value) ->
    lists:any(fun(Item) -> Item =:= Value end, List).

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
%% Gets the digits numbers from a string.
%% @param Value The value to check.
%% @returns A list with the digits.
%% @end
%%-----------------------------------------------------------------------
get_digits(Victim) when is_list(Victim) ->
    [C - $0 || C <- Victim, $0 =< C, C =< $9];
get_digits(Victim) when is_integer(Victim) ->
    get_digits(integer_to_list(Victim));
get_digits(_) ->
    throw({get_digits, "Type not supported."}).

%%-----------------------------------------------------------------------
%% @doc
%% Converts a substring into a integer number.
%% @param Value The string to convert.
%% @returns A tuple with the integer numbers and the rest of the string.
%% @end
%%-----------------------------------------------------------------------
get_integer(Value) ->
    get_integer(Value, "", []).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun get_integer/1'.
%% @end
%%-----------------------------------------------------------------------
get_integer("", Rest, Digits) ->
    case lists:reverse(Digits) of
        [] ->
            {none, Rest};
        Number ->
            {list_to_integer(Number), Rest}
    end;
get_integer([Character | String], Rest, Digits) ->
    case {$0 =< Character andalso Character =< $9, Digits} of
        {true, _} ->
            get_integer(String, Rest, [Character | Digits]);
        {false, []} ->
            get_integer(String, Rest, Digits);
        {false, _} ->
            get_integer("", String, Digits)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Converts a string into a list of integer numbers.
%% @param Value The string to convert.
%% @returns The list with the integer numbers.
%% @end
%%-----------------------------------------------------------------------
get_integers(Value) ->
    get_integers(Value, []).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun get_integers/1'.
%% @end
%%-----------------------------------------------------------------------
get_integers("", Result) ->
    lists:reverse(Result);
get_integers(Value, Result) ->
    case get_integer(Value) of
        {none, NextValue} ->
            get_integers(NextValue, Result);
        {Number, NextValue} ->
            get_integers(NextValue, [Number | Result])
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Selects elements from a list with a list of selectors.
%% @param List The list to check.
%% @param Indexes The list with the indexes.
%% @returns The list with the selected elements.
%% @end
%%-----------------------------------------------------------------------
select([], _) ->
    [];
select(List, Indexes) ->
    lists:flatten(lists:map(
        fun ({Begin, End}) when is_integer(Begin), is_integer(End) ->
                lists:sublist(List, Begin, (End - Begin) + 1);
            (Index) when is_integer(Index) ->
                lists:nth(Index, List);
            (_) ->
                []
        end,
        Indexes
    )).

%%-----------------------------------------------------------------------
%% @doc
%% Selects elements from a list with a list of selectors.
%% @param List The list to check.
%% @param String The string with the indexes.
%% @returns The list with the selected elements.
%% @end
%%-----------------------------------------------------------------------
select_with_string([], _) ->
    [];
select_with_string(List, String) ->
    Indexes = lists:map(
        fun (Substring) ->
            case get_integers(Substring) of
                [Begin, End] -> {Begin, End};
                [Index|_] -> Index;
                _ -> error
            end
        end,
        string:split(String, ",", all)
    ),
    select(List, Indexes).

%%%======================================================================
%%% Loop functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Executes a loop with a range of numbers.
%% @param Index The start value of the range.
%% @param Limit The limit value of the range.
%% @param Function The function to execute each iteration.
%% @returns 'nothing' after complete all the iterations;
%% otherwise 'break' o a tuple {'return', Value}.
%% @end
%%-----------------------------------------------------------------------
forward(Index, Limit, Function) when Index < Limit ->
    case Function(Index) of
        break -> break;
        {return, Value} -> {return, Value};
        _ -> forward(Index + 1, Limit, Function)
    end;
forward(_, _, _) ->
    nothing.

%%-----------------------------------------------------------------------
%% @doc
%% Executes a loop with a range of numbers.
%% @param Index The start value of the range.
%% @param Limit The limit value of the range.
%% @param State The state value of the loop.
%% @param Function The function to execute each iteration.
%% @returns The final state after complete all the iterations;
%% otherwise 'break' o a tuple {'return', Value}.
%% @end
%%-----------------------------------------------------------------------
forward(Index, Limit, State, Function) when Index < Limit ->
    case Function(Index, State) of
        break -> break;
        {return, Value} -> {return, Value};
        NextState -> forward(Index + 1, Limit, NextState, Function)
    end;
forward(_, _, State, _) ->
    State.

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
%%% Mixture functions
%%%======================================================================

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
