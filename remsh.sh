#!/bin/bash
set -x

if [ -z "$1" ]; then
    HN=`hostname -s`
    DIST_NAME="-sname"
    NAME="remsh"
else
    HN=`hostname`
    DIST_NAME="$1"
    NAME="remsh@$HN"
fi

erl $DIST_NAME $NAME -setcookie goanna -remsh "goanna@$HN" -hidden