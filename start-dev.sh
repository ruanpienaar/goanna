#!/bin/sh

# -mnesia dir "'"$PWD"/Mnesia'"
rebar clean app=goanna
rebar compile
cd `dirname $0`
<<<<<<< 178d17ff990ea251bd2841afc12415b2d18af2b5
erl +A 1 +K true -sname goanna -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -cookie goanna -s goanna_api start -hidden
=======
erl +A 1 +K true -name goanna -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -setcookie goanna -s goanna_api start -hidden
>>>>>>> Update start-dev.sh
