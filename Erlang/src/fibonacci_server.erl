%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% This server calculates the Fibonacci's numbers sequence.
%%% @end
%%%======================================================================
-module(fibonacci_server).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([
    % Public functions:
    start_link/0, iterator/0, next/1, reset/0,

    % gen_server callbacks:
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {Number, NextIndex} = gen_server:call(?SERVER, {get_number, Index}),
    {Number, {?SERVER, NextIndex}}.

%%-----------------------------------------------------------------------
%% @doc
%% Resets the internal data of the server.
%% @end
%%-----------------------------------------------------------------------
reset() ->
    gen_server:cast(?SERVER, reset).

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
%%% gen_server callbacks
%%%======================================================================

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%%-----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% @end
%%-----------------------------------------------------------------------
handle_call({get_number, Index}, _From, State) when Index =< 0 ->
    {reply, {0, 1}, State};
handle_call({get_number, Index}, _From, State = #state{table = Table}) ->
    {Number, NextTable} = get_number(Table, Index),
    {reply, {Number, Index + 1}, State#state{table = NextTable}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%% @end
%%-----------------------------------------------------------------------
handle_cast(reset, _State) ->
    {noreply, #state{}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% @end
%%-----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do
%% any necessary cleaning up. When it returns, the gen_server
%% terminates with Reason. The return value is ignored.
%% @end
%%-----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
%% @end
%%-----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
