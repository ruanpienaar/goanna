#!/bin/bash
./rebar3 clean
./rebar3 compile
./rebar3 eunit skip_deps=true
DEBUG=1 ./rebar3 coveralls send