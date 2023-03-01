#!/bin/bash

# ================================================================
# run interactive
# ================================================================
run_interactive() {
    erl -pa out/production/Erlang
}

# ================================================================
# run compile module
# ================================================================
run_compile() {
    mkdir -p out/production/Erlang
    echo ">>> Compiling file: src/$1.erl"
    erlc -pa out/production/Erlang -o out/production/Erlang src/$1.erl
}

# ================================================================
# run build
# ================================================================
run_build() {
    mkdir -p out/production/Erlang
    for file in src/*.erl; do
        echo ">>> Compiling file: $file"
        erlc -pa out/production/Erlang -o out/production/Erlang $file
    done
}

# ================================================================
# run make
# ================================================================
run_make() {
    files=`ls src/*.erl`
    mkdir -p out/production/Erlang
    erlc -pa out/production/Erlang -o out/production/Erlang src/singleton.erl
    erlc -pa out/production/Erlang -o out/production/Erlang $files
}

# ================================================================
# run problem module
# ================================================================
run_problem() {
    erl -pa out/production/Erlang -noshell -run $1 main -s init stop
}

# ================================================================
# run test number
# ================================================================
run_test() {
    erl -pa out/production/Erlang -noshell -eval "eunit:test(tests, [verbose])" -s init stop -extra "$1"
}

# ================================================================
# run help
# ================================================================
run_help() {
    echo "Options:"
    echo "run -i        = Erlang shell"
    echo "run -m        = Builds all the modules"
    echo "run -c module = Compiles a module"
    echo "run -p module = Runs a module"
    echo "run -t number = Runs a test"
}

# ================================================================
# main
# ================================================================
if [ $# -eq 1 ]; then
    if [[ "$1" == "-h" ]] || [[ "$1" == "help" ]]; then
        run_help
    elif [[ "$1" == "-i" ]] || [[ "$1" == "interactive" ]]; then
        run_interactive
    elif [[ "$1" == "-b" ]] || [[ "$1" == "build" ]]; then
        run_build
    elif [[ "$1" == "-m" ]] || [[ "$1" == "make" ]]; then
        run_make
    else
        run_problem $1
    fi
elif [ $# -eq 2 ]; then
    if [[ "$1" == "-c" ]] || [[ "$1" == "compile" ]]; then
        run_compile $2
    elif [[ "$1" == "-p" ]] || [[ "$1" == "problem" ]]; then
        run_problem $2
    elif [[ "$1" == "-t" ]] || [[ "$1" == "test" ]]; then
        run_test $2
    else
        run_help
    fi
else
    run_help
fi
