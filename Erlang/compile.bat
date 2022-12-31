@echo off
mkdir out\production\Erlang 2> nul
echo | set /p=">>> Compiling file: src\%1.erl"
erlc -o out\production\Erlang src\%1.erl
