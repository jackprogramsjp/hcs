@echo off
setlocal EnableDelayedExpansion

rem Haven't tested in Windows yet

for %%f in (.\src\*.hs) do (
    hindent %%f
)

for %%f in (.\app\*.hs) do (
    hindent %%f
)

for %%f in (.\test\*.hs) do (
    hindent %%f
)
