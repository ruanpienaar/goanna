#!/bin/sh
# -mnesia dir "'"$PWD"/Mnesia'"
set -x

HN=`hostname`
if [ -z "$1" ]; then
    DIST_NAME="-sname"
    NAME="goanna"
else
    DIST_NAME="$1"
    NAME="goanna"
fi

cd `dirname $0`
erl +A 1 +K true $DIST_NAME $NAME -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -setcookie goanna -s goanna_api start -noshell -noinput -detached -proto_dist hawk_tcp
#-hidden