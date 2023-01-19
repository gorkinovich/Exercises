%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% This server calculates the sequence of prime numbers.
%%% @end
%%%======================================================================
-module(primes).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([
    % Public functions:
    start_link/0, get/0, get/1, iterator/0, next/1, reset/0,

    % gen_server callbacks:
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

%%%======================================================================
%%% Macros
%%%======================================================================

-define(SERVER, ?MODULE).
-define(OFFSET, 2).
-record(state, { primes = [] }).

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
%% Gets a prime number.
%% @returns The prime number.
%% @end
%%-----------------------------------------------------------------------
get() ->
    gen_server:call(?SERVER, get_number).

%%-----------------------------------------------------------------------
%% @doc
%% Gets a prime number.
%% @param Index The number to get.
%% @returns The prime number.
%% @end
%%-----------------------------------------------------------------------
get(Index) ->
    gen_server:call(?SERVER, {get_number, Index}).

%%-----------------------------------------------------------------------
%% @doc
%% Makes a new sequence of numbers iterator.
%% @returns The new sequence iterator.
%% @end
%%-----------------------------------------------------------------------
iterator() ->
    {?SERVER, 0}.

%%-----------------------------------------------------------------------
%% @doc
%% Gets the next number in the sequence of prime numbers.
%% @param Iterator The iterator handler.
%% @returns A tuple with the current number and the updated iterator.
%% @end
%%-----------------------------------------------------------------------
next({?SERVER, Index}) ->
    {Number, NextIndex} = gen_server:call(?SERVER, {get_next, Index}),
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
%% Gets the next number in the sequence of prime numbers.
%% @param Numbers The list with the previous prime numbers.
%% @param Index The index value to query inside the sequence.
%% @returns A tuple with the current number and the updated list.
%% @end
%%-----------------------------------------------------------------------
get_number(Primes, Index) when Index >= 0 ->
    Length = length(Primes),
    case Length > Index of
        true ->
            Prime = lists:nth(Index + 1, Primes),
            {Prime, Primes};
        _ ->
            next_primes((1 + Index) - Length, Primes, 0)
    end;
get_number(Primes, _) ->
    {undefined, Primes}.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the next prime number in the sequence.
%% @param Primes The previous prime numbers.
%% @returns A tuple with the next prime and the new list of primes.
%% @end
%%-----------------------------------------------------------------------
next_prime([]) ->
    {2, [2]};
next_prime([2]) ->
    {3, [2, 3]};
next_prime(Primes) ->
    next_prime(lists:last(Primes) + ?OFFSET, Primes).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets the next prime number in the sequence.
%% @param Number The numbers to check.
%% @param Primes The previous prime numbers.
%% @returns A tuple with the next prime and the new list of primes.
%% @end
%%-----------------------------------------------------------------------
next_prime(Number, Primes) ->
    case lists:any(fun(Prime) -> (Number rem Prime) =:= 0 end, Primes) of
        false ->
            {Number, Primes ++ [Number]};
        _ ->
            next_prime(Number + ?OFFSET, Primes)
    end.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets an amount of prime numbers in the sequence.
%% @param Amount The amount of numbers to get.
%% @param Primes The previous prime numbers.
%% @param Last The last prime number generated.
%% @returns The new list of prime numbers.
%% @end
%%-----------------------------------------------------------------------
next_primes(Amount, Primes, Last) when Amount < 1 ->
    {Last, Primes};
next_primes(Amount, Primes, Last) ->
    {NextLast, NextPrimes} = case Last < 3 of
                                 true -> next_prime(Primes);
                                 _ -> next_prime(Last + ?OFFSET, Primes)
                             end,
    next_primes(Amount - 1, NextPrimes, NextLast).

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
handle_call(get_number, _From, State = #state{primes = Primes}) ->
    {Number, NextPrimes} = next_prime(Primes),
    {reply, Number, State#state{primes = NextPrimes}};
handle_call({get_number, Index}, _From, State = #state{primes = Primes}) ->
    {Number, NextPrimes} = get_number(Primes, Index),
    {reply, Number, State#state{primes = NextPrimes}};
handle_call({get_next, Index}, _From, State = #state{primes = Primes}) ->
    case Index >= 0 of
        false ->
            {reply, {undefined, 0}, State};
        _ ->
            {Number, NextPrimes} = get_number(Primes, Index),
            {reply, {Number, Index + 1}, State#state{primes = NextPrimes}}
    end;
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
