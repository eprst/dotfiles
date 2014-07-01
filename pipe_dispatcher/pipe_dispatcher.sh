#!/bin/bash

pipe_dir=/var/execpipes

for pipe in `ls $pipe_dir`
do
  pipe="$pipe_dir/$pipe"
  if [[ -p $pipe ]]; then
    echo $@ | /usr/bin/pipe_echo $pipe
  fi
done
