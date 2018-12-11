#!/bin/sh
cd frontend;
while [ 1 ]; do
  inotifywait -e MODIFY $(find . -name "*.purs")
  pulp browserify > ../static/main.js
done
