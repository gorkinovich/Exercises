%%%======================================================================
%%% @author Gorka Suárez García
%%% @copyright (C) 2022-2023, Gorka Suárez García
%%% @doc
%%% This module contains tests functions.
%%% @end
%%%======================================================================
-module(tests).
-author("Gorka Suárez García").
-include_lib("eunit/include/eunit.hrl").

-define(TEST(Desc, Func), {setup, fun setup/0, fun cleanup/1, {Desc, {timeout, 60, Func}}}).

setup() -> ok.
cleanup(_) -> ok.

fixtures() ->
    [
        ?TEST("Problem 001", ?_assertMatch(233168, pe001:result())),
        ?TEST("Problem 002", ?_assertMatch(4613732, pe002:result())),
        ?TEST("Problem 003", ?_assertMatch(6857, pe003:result())),
        ?TEST("Problem 004", ?_assertMatch({906609, 913, 993}, pe004:result())),
        ?TEST("Problem 005", ?_assertMatch(232792560, pe005:result())),
        ?TEST("Problem 006", ?_assertMatch(25164150, pe006:result())),
        ?TEST("Problem 007", ?_assertMatch(104743, pe007:result())),
        ?TEST("Problem 008", ?_assertMatch(23514624000, pe008:result())),
        ?TEST("Problem 009", ?_assertMatch({200, 375, 425}, pe009:result())),
        ?TEST("Problem 010", ?_assertMatch(142913828922, pe010:result())),
        ?TEST("Problem 011", ?_assertMatch(70600674, pe011:result())),
        ?TEST("Problem 012", ?_assertMatch(76576500, pe012:result())),
        ?TEST("Problem 013", ?_assertMatch("5537376230", pe013:result())),
        ?TEST("Problem 014", ?_assertMatch(837799, pe014:result())),
        ?TEST("Problem 015", ?_assertMatch(137846528820, pe015:result())),
        ?TEST("Problem 016", ?_assertMatch(1366, pe016:result())),
        ?TEST("Problem 017", ?_assertMatch(21124, pe017:result()))
        % ?TEST("Problem 018", ?_assertMatch({1074, [75, 64, 82, 87, 82, 75, 73, 28, 83, 32, 91, 78, 58, 73, 93]}, pe018:result()))
        % ?TEST("Problem 019", ?_assertMatch(171, pe019:result()))
        % ?TEST("Problem 020", ?_assertMatch(648, pe020:result()))
        % ?TEST("Problem 021", ?_assertMatch({31626, [220, 284, 1184, 1210, 2620, 2924, 5020, 5564, 6232, 6368]}, pe021:result()))
        % ?TEST("Problem 022", ?_assertMatch(871198282, pe022:result()))
        % ?TEST("Problem 023", ?_assertMatch(4179871, pe023:result()))
        % ?TEST("Problem 024", ?_assertMatch(2783915460, pe024:result()))
        % ?TEST("Problem 025", ?_assertMatch(1070066266382758936764980584457396885083683896632151665013235203375314520604694040621889147582489792657804694888177591957484336466672569959512996030461262748092482186144069433051234774442750273781753087579391666192149259186759553966422837148943113074699503439547001985432609723067290192870526447243726117715821825548491120525013201478612965931381792235559657452039506137551467837543229119602129934048260706175397706847068202895486902666185435124521900369480641357447470911707619766945691070098024393439617474103736912503231365532164773697023167755051595173518460579954919410967778373229665796581646513903488154256310184224190259846088000110186255550245493937113651657039447629584714548523425950428582425306083544435428212611008992863795048006894330309773217834864543113205765659868456288616808718693835297350643986297640660000723562917905207051164077614812491885830945940566688339109350944456576357666151619317753792891661581327159616877487983821820492520348473874384736771934512787029218636250627816, pe025:result()))
        % ?TEST("Problem 026", ?_assertMatch(983, pe026:result()))
        % ?TEST("Problem 027", ?_assertMatch({1, 41, [41, 43, 47, 53, 61, 71, 83, 97, 113, 131, 151, 173, 197, 223, 251, 281, 313, 347, 383, 421, 461, 503, 547, 593, 641, 691, 743, 797, 853, 911, 971, 1033, 1097, 1163, 1231, 1301, 1373, 1447, 1523, 1601]}, pe027:result()))
        % ?TEST("Problem 028", ?_assertMatch(669171001, pe028:result()))
        % ?TEST("Problem 029", ?_assertMatch(9183, pe029:result()))
        % ?TEST("Problem 030", ?_assertMatch(240559, pe030:result()))
    ].

main_test_() ->
    Tests = fixtures(),
    case init:get_plain_arguments() of
        [Argument|_] ->
            tools:select_with_string(Tests, Argument);
        _ ->
            Tests
    end.
