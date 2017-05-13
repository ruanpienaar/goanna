#!/bin/bash

# Original
# rebar compile && rebar skip_deps=true eunit && rebar skip_deps=true ct
rebar3 clean 
rebar3 compile 
rebar3 eunit skip_deps=true