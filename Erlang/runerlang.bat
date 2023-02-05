@echo off
erl -pa out\production\Erlang -noshell -run %1 main -s init stop
