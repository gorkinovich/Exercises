%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% 2520 is the smallest number that can be divided by each of the
%%% numbers from 1 to 10 without any remainder.
%%%
%%% What is the smallest positive number that is evenly divisible
%%% (divisible with no remainder) by all of the numbers from 1 to 20?
%%% @end
%%%======================================================================
-module(pe005).
-author("Gorka Suárez García").
-export([main/0]).

-define(FIRST, 1).
-define(LAST, 20).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    {_, Result} = find_number(?FIRST, ?LAST),
    io:format("The smallest positive number that is a multiple of all "
              "numbers from 1 to 20 is ~p.~n", [Result]).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Find the smallest natural divisible by a range of numbers.
%% @param First The first value of the divisors range.
%% @param Last The last value of the divisors range.
%% @returns The smallest natural number.
%% @end
%%-----------------------------------------------------------------------
find_number(First, Last) ->
    Numbers = lists:seq(First, Last),
    utils:find(
        lists:seq(1, length(Numbers)),
        fun(Size) ->
            utils:find(
                utils:combinations(Numbers, Size),
                fun(Combination) ->
                    X = utils:product(Combination),
                    case utils:all(Numbers, fun(Y) -> (X rem Y) == 0 end) of
                        true -> {ok, X};
                        _ -> continue
                    end
                end
            )
        end
    ).
