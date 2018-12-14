#!/bin/sh
while [ 1 ]; do
  bash -c 'echo $$ > pid; exec ./magic-maze-backend 3030'
  inotifywait -e MODIFY $(find backend -name "*.hs")
  stack build --copy-bins
  kill -s INT $(cat pid | tr -d '\n')
done
