%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This module represents a vector of elements.
%%% @end
%%%======================================================================
-module(vector).
-author("Gorka Suárez García").
-export([
    new/0, new/1, count/1, get/2, get/3, set/3, first/1,
    first/2, last/1, last/2, append/2, to_list/1
]).

%%-----------------------------------------------------------------------
%% @doc
%% Creates a new empty vector.
%% @returns The created vector.
%% @end
%%-----------------------------------------------------------------------
new() ->
    #{}.

%%-----------------------------------------------------------------------
%% @doc
%% Creates a new vector with an initialization.
%% @param Elements The elements used in the initialization.
%% @returns The created vector with the elements.
%% @end
%%-----------------------------------------------------------------------
new(List) when is_list(List) ->
    lists:foldl(
        fun({Index, Item}, Map) ->
            Map#{ Index => Item }
        end,
        #{},
        lists:zip(lists:seq(0, length(List) - 1), List)
    );
new(Element) ->
    #{ 0 => Element }.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the size of a vector.
%% @param Vector The vector to check.
%% @returns The number of elements.
%% @end
%%-----------------------------------------------------------------------
count(Vector) ->
    maps:size(Vector).

%%-----------------------------------------------------------------------
%% @doc
%% Gets an element inside a vector.
%% @param Vector The vector to check.
%% @param Index The index of the element.
%% @returns The element if found; otherwise 'undefined'.
%% @end
%%-----------------------------------------------------------------------
get(Vector, Index) ->
    maps:get(Index, Vector, undefined).

%%-----------------------------------------------------------------------
%% @doc
%% Gets an element inside a vector.
%% @param Vector The vector to check.
%% @param Index The index of the element.
%% @param Default The default value.
%% @returns The element if found; otherwise the default value.
%% @end
%%-----------------------------------------------------------------------
get(Vector, Index, Default) ->
    maps:get(Index, Vector, Default).

%%-----------------------------------------------------------------------
%% @doc
%% Sets an element inside a vector.
%% @param Vector The vector to modify.
%% @param Index The index of the element.
%% @param Value The value to use.
%% @returns The modified vector.
%% @end
%%-----------------------------------------------------------------------
set(Vector, Index, Value) ->
    Vector#{ Index => Value }.

%%-----------------------------------------------------------------------
%% @doc
%% Appends an element inside a vector.
%% @param Vector The vector to modify.
%% @param Value The value to use.
%% @returns The modified vector.
%% @end
%%-----------------------------------------------------------------------
append(Vector, Value) ->
    Index = maps:size(Vector),
    Vector#{ Index => Value }.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the first element inside a vector.
%% @param Vector The vector to check.
%% @returns The element if found; otherwise 'undefined'.
%% @end
%%-----------------------------------------------------------------------
first(Vector) ->
    get(Vector, 0).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the first element inside a vector.
%% @param Vector The vector to check.
%% @param Default The default value.
%% @returns The element if found; otherwise 'undefined'.
%% @end
%%-----------------------------------------------------------------------
first(Vector, Default) ->
    get(Vector, 0, Default).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the first element inside a vector.
%% @param Vector The vector to check.
%% @returns The element if found; otherwise 'undefined'.
%% @end
%%-----------------------------------------------------------------------
last(Vector) ->
    get(Vector, count(Vector) - 1).

%%-----------------------------------------------------------------------
%% @doc
%% Gets the first element inside a vector.
%% @param Vector The vector to check.
%% @param Default The default value.
%% @returns The element if found; otherwise 'undefined'.
%% @end
%%-----------------------------------------------------------------------
last(Vector, Default) ->
    get(Vector, count(Vector) - 1, Default).

%%-----------------------------------------------------------------------
%% @doc
%% Converts a vector into a standard list.
%% @param Vector The vector to check.
%% @returns A list with the elements.
%% @end
%%-----------------------------------------------------------------------
to_list(Vector) ->
    Elements = lists:foldl(
        fun(Item, Result) ->
            [get(Vector, Item) | Result]
        end,
        [],
        lists:seq(0, count(Vector) - 1)
    ),
    lists:reverse(Elements).
