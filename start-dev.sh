#!/bin/sh
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
run_erl -daemon pipe/ log/ "exec erl +A 1 +K true $DIST_NAME $NAME -boot start_sasl -config $PWD/sys.config -pa $PWD/_build/default/lib/*/ebin $PWD/test -setcookie goanna -s goanna_api start -proto_dist hawk_tcp -hidden +Bi" &
#sleep 1
#./console.sh
