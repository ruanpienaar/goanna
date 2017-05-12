#!/bin/bash
HN=`hostname`
PID=`ps auxwww | grep "goanna" | grep -v grep | awk '{ print $2 }'`
if [ ! -z "$PID" ]; then
    set -x
    kill $PID
fi
