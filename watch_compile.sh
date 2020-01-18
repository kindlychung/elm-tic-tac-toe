#!/usr/bin/env bash

elm make src/Main.elm  --output main.js
while true; do
inotifywait -e modify,create,delete -r src ./elm.json ./*.html ./*.js && \
  elm make src/Main.elm  --output main.js
  sleep 1
done
