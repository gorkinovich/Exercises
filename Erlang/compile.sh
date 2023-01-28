#!/bin/bash
mkdir -p out/production/Erlang
echo ">>> Compiling file: src/$1.erl"
erlc -pa out/production/Erlang -o out/production/Erlang src/$1.erl
