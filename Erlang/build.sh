#!/bin/bash
mkdir -p out/production/Erlang
for file in src/*.erl; do
    echo ">>> Compiling file: $file"
    erlc -o out/production/Erlang $file
done
