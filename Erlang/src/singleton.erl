%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% This server handles data as a singleton pattern.
%%% @end
%%%======================================================================
-module(singleton).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([
    % Public functions:
    start_link/0, get/1, get/2, set/2, apply/2,

    % gen_server callbacks:
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

%%%======================================================================
%%% Macros
%%%======================================================================

-define(SERVER, ?MODULE).
-define(UNDEFINED, undefined).
-record(state, { table = #{} }).

%%%======================================================================
%%% Public functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Spawns the server and registers the unique local name.
%% @end
%%-----------------------------------------------------------------------
start_link() ->
    case whereis(?SERVER) of
        undefined ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
        PID ->
            {ok, PID}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Gets a variable in the singleton.
%% @param Name The name of the variable.
%% @returns The value of the variable.
%% @end
%%-----------------------------------------------------------------------
get(Name) ->
    gen_server:call(?SERVER, {get, Name, ?UNDEFINED}).

%%-----------------------------------------------------------------------
%% @doc
%% Gets a variable in the singleton.
%% @param Name The name of the variable.
%% @param Default The default value if the variable doesn't exist.
%% @returns The value of the variable or the default value.
%% @end
%%-----------------------------------------------------------------------
get(Name, Default) ->
    gen_server:call(?SERVER, {get, Name, Default}).

%%-----------------------------------------------------------------------
%% @doc
%% Sets a variable in the singleton.
%% @param Name The name of the variable.
%% @param Value The value for the variable.
%% @end
%%-----------------------------------------------------------------------
set(Name, Value) ->
    gen_server:cast(?SERVER, {set, Name, Value}).

%%-----------------------------------------------------------------------
%% @doc
%% Applies a function to a variable in the singleton.
%% @param Name The name of the variable.
%% @param Function The function to apply.
%% @end
%%-----------------------------------------------------------------------
apply(Name, Function) ->
    gen_server:cast(?SERVER, {apply, Name, Function}).

%%%======================================================================
%%% Internal functions
%%%======================================================================

update_table(State, Name, Value) ->
    Table = State#state.table,
    Table#{ Name => Value }.

%%%======================================================================
%%% gen_server callbacks
%%%======================================================================

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event init.
%% @end
%%-----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle call messages.
%% @end
%%-----------------------------------------------------------------------
handle_call({get, Name, Default}, _From, State) ->
    {reply, maps:get(Name, State#state.table, Default), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle cast messages.
%% @end
%%-----------------------------------------------------------------------
handle_cast({set, Name, Value}, State) ->
    NextTable = update_table(State, Name, Value),
    {noreply, State#state{ table = NextTable }};
handle_cast({apply, Name, Function}, State) ->
    case State#state.table of
        #{ Name := Value } ->
            NextTable = update_table(State, Name, Function(Value)),
            {noreply, State#state{ table = NextTable }};
        _ ->
            {noreply, State}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle non call/cast messages.
%% @end
%%-----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event terminate.
%% @end
%%-----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event code change.
%% @end
%%-----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
