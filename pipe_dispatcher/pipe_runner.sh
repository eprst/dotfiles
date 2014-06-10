#!/bin/bash

pipe="/var/execpipes/pipe_$$"

cleanup() {
  rm -f $pipe
  exit $?
}

trap cleanup EXIT
trap cleanup SIGINT

mkfifo $pipe

while true
do
    if read line <$pipe; then
        if [[ "$line" == 'quit' ]]; then
            break
        fi
        ( eval "$line" )
    fi
done
