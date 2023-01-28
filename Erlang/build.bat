@echo off
mkdir out\production\Erlang 2> nul
for /r %%f in (src\*.erl) do (
    echo | set /p=">>> Compiling file: %%f"
    echo:
    erlc -pa out\production\Erlang -o out\production\Erlang %%f
)
