#!/bin/sh

# -mnesia dir "'"$PWD"/Mnesia'"
rebar clean app=goanna
rebar compile
cd `dirname $0`
erl +A 1 +K true -sname goanna -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -cookie goanna -s goanna_api start