%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This server calculates the Fibonacci's numbers sequence.
%%% @end
%%%======================================================================
-module(fibonacci).
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
-record(state, { table = #{0 => 0, 1 => 1} }).

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
%% Gets a Fibonacci number.
%% @param Index The number to get.
%% @returns The Fibonacci number.
%% @end
%%-----------------------------------------------------------------------
get(Index) ->
    singleton:call(?SERVER, {get_number, Index}).

%%-----------------------------------------------------------------------
%% @doc
%% Makes a new Fibonacci's number sequence iterator.
%% @returns The new sequence iterator.
%% @end
%%-----------------------------------------------------------------------
iterator() ->
    {?SERVER, 0}.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the next number in the Fibonacci's number sequence.
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
%% Gets the next number in the Fibonacci's number sequence.
%% @param Table The table with the previous Fibonacci's numbers.
%% @param Index The index value to query inside the sequence.
%% @returns A tuple with the current number and the updated table.
%% @end
%%-----------------------------------------------------------------------
get_number(Table, Index) when Index >= 0 ->
    case maps:get(Index, Table, none) of
        none ->
            {N1, T1} = get_number(Table, Index - 1),
            {N2, T2} = get_number(T1, Index - 2),
            NextTable = T2#{ Index => N1 + N2 },
            {N1 + N2, NextTable};
        Number ->
            {Number, Table}
    end;
get_number(Table, _) ->
    {0, Table}.

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
handle_call({get_number, Index}, _From, State = #state{table = Table}) ->
    {Number, NextTable} = get_number(Table, Index),
    {reply, Number, State#state{table = NextTable}};
handle_call({get_next, Index}, _From, State) when Index =< 0 ->
    {reply, {0, 1}, State};
handle_call({get_next, Index}, _From, State = #state{table = Table}) ->
    {Number, NextTable} = get_number(Table, Index),
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
