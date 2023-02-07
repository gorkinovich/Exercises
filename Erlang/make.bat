@echo off
SETLOCAL ENABLEDELAYEDEXPANSION
SET "files="
FOR /f "delims=" %%i IN ('dir /b /s "src\*.erl"') DO (
    SET files=!files! "%%i"
)
mkdir out\production\Erlang 2> nul
erlc -pa out\production\Erlang -o out\production\Erlang src\singleton.erl
erlc -pa out\production\Erlang -o out\production\Erlang %files%
