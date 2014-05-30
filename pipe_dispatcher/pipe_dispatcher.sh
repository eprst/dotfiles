#!/bin/bash

pipe_dir=/var/run/execpipes

for pipe in `ls $pipe_dir`
do
  pipe="$pipe_dir/$pipe"
  if [[ -p $pipe ]]; then
    echo $@ > $pipe
  fi
done
