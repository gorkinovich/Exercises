@echo off
if [%1]==[-h] goto help
if [%1]==[-i] goto interactive
if [%1]==[-c] goto compile
if [%1]==[-b] goto build
if [%1]==[-m] goto make
if [%1]==[-p] goto problem
if [%1]==[-t] goto test

if [%1]==[help] goto help
if [%1]==[interactive] goto interactive
if [%1]==[compile] goto compile
if [%1]==[build] goto build
if [%1]==[make] goto make
if [%1]==[problem] goto problem
if [%1]==[test] goto test

rem ================================================================
rem run module
rem ================================================================
if [%1]==[] goto help

erl -pa out\production\Erlang -noshell -run %1 main -s init stop

goto end

rem ================================================================
rem run interactive
rem ================================================================
:interactive

werl -pa out\production\Erlang

goto end

rem ================================================================
rem run compile module
rem ================================================================
:compile
if [%2]==[] goto help

mkdir out\production\Erlang 2> nul
echo | set /p=">>> Compiling file: src\%2.erl"
erlc -pa out\production\Erlang -o out\production\Erlang src\%2.erl

goto end

rem ================================================================
rem run build
rem ================================================================
:build

mkdir out\production\Erlang 2> nul
for /r %%f in (src\*.erl) do (
    echo | set /p=">>> Compiling file: %%f"
    echo:
    erlc -pa out\production\Erlang -o out\production\Erlang %%f
)

goto end

rem ================================================================
rem run make
rem ================================================================
:make

SETLOCAL ENABLEDELAYEDEXPANSION
SET "files="
FOR /f "delims=" %%i IN ('dir /b /s "src\*.erl"') DO (
    SET files=!files! "%%i"
)
mkdir out\production\Erlang 2> nul
erlc -pa out\production\Erlang -o out\production\Erlang src\singleton.erl
erlc -pa out\production\Erlang -o out\production\Erlang %files%

goto end

rem ================================================================
rem run problem module
rem ================================================================
:problem
if [%2]==[] goto help

erl -pa out\production\Erlang -noshell -run %2 main -s init stop

goto end

rem ================================================================
rem run test number
rem ================================================================
:test
if [%2]==[] goto help

erl -pa out\production\Erlang -noshell -eval "eunit:test(tests, [verbose])" -s init stop -extra "%2"

goto end

rem ================================================================
rem run help
rem ================================================================
:help

echo Options:
echo run -i        = Erlang shell
echo run -m        = Builds all the modules
echo run -c module = Compiles a module
echo run -p module = Runs a module
echo run -t number = Runs a test

:end
