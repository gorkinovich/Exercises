%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This server calculates the triangle numbers sequence.
%%% @end
%%%======================================================================
-module(triangle).
-author("Gorka Suárez García").
-behaviour(singleton).
-export([
    % Public functions:
    start_link/0, get/1, iterator/0, next/1, reset/0,

    % singleton callbacks:
    handle_call/3, handle_cast/2
]).

%%%======================================================================
%%% Macros
%%%======================================================================

-define(SERVER, ?MODULE).
-record(state, { table = vector:new([0]) }).

%%%======================================================================
%%% Public functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Spawns the server and registers the unique local name.
%% @end
%%-----------------------------------------------------------------------
start_link() ->
    singleton:start_link(?SERVER, #state{}).

%%-----------------------------------------------------------------------
%% @doc
%% Gets a triangle number.
%% @param Index The number to get.
%% @returns The triangle number.
%% @end
%%-----------------------------------------------------------------------
get(Index) ->
    singleton:call(?SERVER, {get_number, Index}).

%%-----------------------------------------------------------------------
%% @doc
%% Makes a new triangle number sequence iterator.
%% @returns The new sequence iterator.
%% @end
%%-----------------------------------------------------------------------
iterator() ->
    {?SERVER, 0}.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the next number in the triangle number sequence.
%% @param Iterator The iterator handler.
%% @returns A tuple with the current number and the updated iterator.
%% @end
%%-----------------------------------------------------------------------
next({?SERVER, Index}) ->
    {Number, NextIndex} = singleton:call(?SERVER, {get_next, Index}),
    {Number, {?SERVER, NextIndex}}.

%%-----------------------------------------------------------------------
%% @doc
%% Resets the internal data of the server.
%% @end
%%-----------------------------------------------------------------------
reset() ->
    singleton:cast(?SERVER, reset).

%%%======================================================================
%%% Internal functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the next number in the triangle number sequence.
%% @param Table The table with the previous triangle numbers.
%% @param Index The index value to query inside the sequence.
%% @returns A tuple with the current number and the updated table.
%% @end
%%-----------------------------------------------------------------------
get_number(Table, Index) when Index >= 0 ->
    case vector:get(Table, Index, none) of
        none ->
            get_number(Table, vector:count(Table), Index);
        Number ->
            {Number, Table}
    end;
get_number(Table, _) ->
    {0, Table}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun get_number/2'.
%% @end
%%-----------------------------------------------------------------------
get_number(Table, I, J) when I =< J ->
    Number = vector:get(Table, I - 1),
    NextTable = vector:set(Table, I, Number + I),
    get_number(NextTable, I + 1, J);
get_number(Table, _, Index) ->
    {vector:get(Table, Index), Table}.

%%%======================================================================
%%% singleton callbacks
%%%======================================================================

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle call messages.
%% @end
%%-----------------------------------------------------------------------
handle_call({get_number, Index}, _From, State) when Index =< 0 ->
    {reply, 0, State};
handle_call({get_number, Index}, _From, State) ->
    {Number, NextTable} = get_number(State#state.table, Index),
    {reply, Number, State#state{table = NextTable}};
handle_call({get_next, Index}, _From, State) when Index =< 0 ->
    {reply, {0, 1}, State};
handle_call({get_next, Index}, _From, State) ->
    {Number, NextTable} = get_number(State#state.table, Index),
    {reply, {Number, Index + 1}, State#state{table = NextTable}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle cast messages.
%% @end
%%-----------------------------------------------------------------------
handle_cast(reset, _State) ->
    {noreply, #state{}};
handle_cast(_Request, State) ->
    {noreply, State}.
