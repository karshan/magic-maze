#!/bin/sh
while [ 1 ]; do
  ./magic-maze-backend 3030 &
  PID=$!
  inotifywait -e MODIFY $(find backend -name "*.hs")
  stack build --copy-bins
  kill -s INT $PID
  sleep 1
done
