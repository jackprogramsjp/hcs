#!/bin/bash
# https://batsh.org

command=$1
function build {
  "echo" "-e" "stack setup"
  "stack" "setup"
  "echo" "-e" "stack build"
  "stack" "build"
}
function exec {
  "echo" "-e" "stack exec hcs-exe"
  "stack" "exec" "hcs-exe"
}
function clean_stack {
  "echo" "-e" "stack clean --full"
  "stack" "clean" "--full"
}
if [ "$command" == "build" ]; then
  "build" 
else
  if [ "$command" == "exec" ]; then
    "exec" 
  else
    if [ "$command" == "clean-stack" ]; then
      "clean_stack" 
    else
      "build" 
      "exec" 
    fi
  fi
fi