#!/bin/bash
./rebar3 clean
./rebar3 compile
rebar3 eunit -c  && rebar3 proper -m goanna_db_tests && rebar3 cover -v