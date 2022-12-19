%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022, Gorka Suárez García
%%% @doc
%%% This module contains math utility functions.
%%% @end
%%%======================================================================
-module(mathutils).
-author("Gorka Suárez García").
-export([factorial/1]).

%%-----------------------------------------------------------------------
%% @doc
%% Calculates the factorial of a natural number.
%% @param Number The number to check.
%% @returns The factorial of the given number.
%% @end
%%-----------------------------------------------------------------------
factorial(Number) -> factorial(Number, 1).

%%-----------------------------------------------------------------------
%% @doc
%% Calculates the factorial of a natural number.
%% @param Number The number to check.
%% @param Accum The accumulated value to return.
%% @returns The factorial of the given number.
%% @end
%%-----------------------------------------------------------------------
factorial(Number, Accum) when Number =< 1 -> Accum;
factorial(Number, Accum) -> factorial(Number - 1, Accum * Number).
