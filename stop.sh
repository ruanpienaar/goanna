#!/bin/bash
#epmd -names
HN=`hostname`
echo `ps auxwww | grep "goanna@$HN"`
PID=`ps auxwww | grep "goanna" | grep -v grep | awk '{ print $2 }'`
echo $PID
kill $PID