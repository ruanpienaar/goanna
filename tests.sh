#!/bin/bash

# Original
# rebar compile && rebar skip_deps=true eunit && rebar skip_deps=true ct
rebar clean compile eunit skip_deps=true