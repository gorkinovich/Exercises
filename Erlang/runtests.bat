@echo off
erl -pa out\production\Erlang -noshell -eval "eunit:test(tests, [verbose])" -s init stop
