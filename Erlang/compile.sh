#!/bin/bash
mkdir -p out/production/Erlang
echo ">>> Compiling file: src/$1.erl"
erlc -o out/production/Erlang src/$1.erl
