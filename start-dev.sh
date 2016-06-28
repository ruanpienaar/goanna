#!/bin/sh

# -mnesia dir "'"$PWD"/Mnesia'"
rebar compile
cd `dirname $0`
exec erl -sname goanna -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -cookie goanna -s goanna start