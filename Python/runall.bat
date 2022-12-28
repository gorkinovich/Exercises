@echo off
setlocal EnableDelayedExpansion
echo ============================================================
if [%1]==[] (
    set "START="
) else (
    set "START=%1"
)
for %%f in (PE*.py) do (
    if [!START!]==[%%f] (
        set "START="
    )
    if [!START!]==[] (
        echo | set /p=">>> RUN: %%f"
        echo:
        echo:
        python %%f
        echo:
        echo ============================================================
    )
)