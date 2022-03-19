@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

rem !/bin/bash
rem https://batsh.org
set command=%1

if !command! EQU build (
  call :build _4 0
  echo | set /p ^=!_4!
) else (
  if !command! EQU exec (
    call :exec _3 0
    echo | set /p ^=!_3!
  ) else (
    if !command! EQU clean-stack (
      call :clean_stack _2 0
      echo | set /p ^=!_2!
    ) else (
      call :build _0 0
      echo | set /p ^=!_0!
      call :exec _1 0
      echo | set /p ^=!_1!
    )
  )
)

goto :EOF
:build
echo stack setup
stack setup
echo stack build
stack build

goto :EOF
:exec
echo stack exec hcs-exe
stack exec hcs-exe

goto :EOF
:clean_stack
echo stack clean --full
stack clean --full