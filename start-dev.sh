#!/bin/sh
# -mnesia dir "'"$PWD"/Mnesia'"

if [ -z "$1" ]; then
    DIST_NAME="-name"
else
    DIST_NAME="$1"
fi

if [ -z "$2" ]; then
    NAME="goanna"
else
    NAME="$2"
fi

#Dev steps:
#./rebar clean app=goanna
#./rebar compile

cd `dirname $0`
erl +A 1 +K true $DIST_NAME $NAME -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -setcookie goanna -s goanna_api start -hidden #-noshell -noinput -detached