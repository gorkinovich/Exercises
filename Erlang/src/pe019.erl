%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% You are given the following information, but you may prefer
%%% to do some research for yourself.
%%%
%%%     + 1 Jan 1900 was a Monday.
%%%     + Thirty days has September,
%%%       April, June and November.
%%%       All the rest have thirty-one,
%%%       Saving February alone,
%%%       Which has twenty-eight, rain or shine.
%%%       And on leap years, twenty-nine.
%%%     + A leap year occurs on any year evenly divisible by 4,
%%%       but not on a century unless it is divisible by 400.
%%%
%%% How many Sundays fell on the first of the month during the
%%% twentieth century (1 Jan 1901 to 31 Dec 2000)?
%%% @end
%%%======================================================================
-module(pe019).
-author("Gorka Suárez García").
-export([main/0, result/0]).

-define(SUNDAY, 7).
-define(DATE_BEGIN, {1901, 1, 1}).
-define(DATE_LIMIT, {2001, 1, 1}).

%%-----------------------------------------------------------------------
%% @doc
%% Main entry for the problem solver.
%% @end
%%-----------------------------------------------------------------------
main() ->
    io:format("The number of Sundays that fell on the month's 1st during the 20th century is ~p.~n", [result()]).

%%-----------------------------------------------------------------------
%% @doc
%% Main result for the problem solver.
%% @end
%%-----------------------------------------------------------------------
result() ->
    Dates = get_weekdays(?DATE_BEGIN, ?DATE_LIMIT, ?SUNDAY),
    length(Dates).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Gets a list of dates of those 1st of the months that
%% fell on a given weekday inside a range of dates.
%% @param Begin The begin date.
%% @param Limit The limit date.
%% @param Weekday The week day to select.
%% @returns The list of days that match with the weekday.
%% @end
%%-----------------------------------------------------------------------
get_weekdays(Begin, Limit, Weekday) ->
    get_weekdays(Begin, Limit, Weekday, []).

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Internal function for 'fun get_weekdays/3'.
%% @end
%%-----------------------------------------------------------------------
get_weekdays(Current = {Year, Month, Day}, Limit, Weekday, Accumulated) ->
    case Current < Limit of
        false ->
            lists:reverse(Accumulated);
        true ->
            Next = case calendar:day_of_the_week(Year, Month, Day) of
                       Weekday -> [Current|Accumulated];
                       _ -> Accumulated
                   end,
            get_weekdays(add_month(Current), Limit, Weekday, Next)
    end.

%%-----------------------------------------------------------------------
%% @private
%% @doc
%% Adds one month to a date.
%% @param Value The date to change.
%% @returns The new date.
%% @end
%%-----------------------------------------------------------------------
add_month({Year, Month, Day}) ->
    case Month < 12 of
        false -> {Year + 1, 1, Day};
        true -> {Year, Month + 1, Day}
    end.
