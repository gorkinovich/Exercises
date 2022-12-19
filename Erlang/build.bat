@echo off
mkdir out\production\Erlang
for /r %%f in (src\*.erl) do (
    echo | set /p=">>> Compiling file: %%f"
    echo:
    erlc -o out\production\Erlang %%f
)
