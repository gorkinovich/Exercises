%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This server handles data as a singleton pattern.
%%% @end
%%%======================================================================
-module(singleton).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([
    % Public functions:
    start_link/2, call/2, cast/2,

    % gen_server callbacks:
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

%%%======================================================================
%%% Callbacks
%%%======================================================================

-callback handle_call(Request :: term(), From :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), Timeout :: term()}
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: term()}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: term()}
    | {stop, Reason :: term(), NewState :: term()}.

%%%======================================================================
%%% Macros
%%%======================================================================

-record(state, { module, data }).

%%%======================================================================
%%% Public functions
%%%======================================================================

%%-----------------------------------------------------------------------
%% @doc
%% Spawns the server and registers the unique local name.
%% @param Module The name of the module spawning the server.
%% @param State The initial state data for the server.
%% @returns The tuple {ok, PID} if everything goes right, otherwise
%% the atom ignore or the tuple {error, Reason} will be returned.
%% @end
%%-----------------------------------------------------------------------
start_link(Module, State) ->
    case whereis(Module) of
        undefined ->
            gen_server:start_link({local, Module}, ?MODULE, [Module, State], []);
        PID ->
            {ok, PID}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Sends a call request to the.
%% @param Module The name of the server.
%% @param Request The request for the server.
%% @returns The result of the request.
%% @end
%%-----------------------------------------------------------------------
call(Module, Request) ->
    gen_server:call(Module, Request).

%%-----------------------------------------------------------------------
%% @doc
%% Sends a cast request to the.
%% @param Module The name of the server.
%% @param Request The request for the server.
%% @end
%%-----------------------------------------------------------------------
cast(Module, Request) ->
    gen_server:cast(Module, Request).

%%%======================================================================
%%% gen_server callbacks
%%%======================================================================

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event init.
%% @end
%%-----------------------------------------------------------------------
init([Module, State]) ->
    {ok, #state{ module = Module, data = State }}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle call messages.
%% @end
%%-----------------------------------------------------------------------
handle_call(Request, From, State) ->
    case apply(State#state.module, handle_call, [Request, From, State#state.data]) of
        {reply, Reply, NewState} ->
            {reply, Reply, State#state{ data = NewState }};
        {reply, Reply, NewState, Other} ->
            {reply, Reply, State#state{ data = NewState }, Other};
        {noreply, NewState} ->
            {noreply, State#state{ data = NewState }};
        {noreply, NewState, Other} ->
            {noreply, State#state{ data = NewState }, Other};
        {stop, Reason, Reply, NewState} ->
            {stop, Reason, Reply, State#state{ data = NewState }};
        {stop, Reason, NewState} ->
            {stop, Reason, State#state{ data = NewState }}
    end.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Generic server event handle cast messages.
%% @end
%%-----------------------------------------------------------------------
handle_cast(Request, State) ->
    case apply(State#state.module, handle_cast, [Request, State#state.data]) of
        {noreply, NewState} ->
            {noreply, State#state{ data = NewState }};
        {noreply, NewState, Other} ->
            {noreply, State#state{ data = NewState }, Other};
        {stop, Reason, NewState} ->
            {stop, Reason, State#state{ data = NewState }}
    end.

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
