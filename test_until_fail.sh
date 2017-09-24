#!/bin/bash

./rebar3 clean
./rebar3 compile

while true; do
  ./rebar3 eunit skip_deps=true
  if [ $? != 0 ]; then
    echo "F"
    exit 1
  else
    echo "S"
  fi
done
