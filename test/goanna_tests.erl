-module(goanna_tests).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test1/1, test2/1]).

all() -> [test1,test2].

test1(_Config) ->
    1=2-1.

test2(_Config) ->
    2=1+1.
