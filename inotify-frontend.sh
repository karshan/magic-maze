#!/bin/sh
cd frontend;
while [ 1 ]; do
  inotifywait -e MODIFY $(find . -name "*.purs")
  pulp browserify -O > ../static/main.js
  notify-send "browserify completed: $?"
done
