#!/bin/bash
./rebar3 clean
./rebar3 compile
./rebar3 eunit skip_deps=true
./rebar3 proper -m goanna_db_tests