-module(goanna_test_module).

-export([
    function/0,
    function2/0,
    function3/1
]).

-spec function() -> ok.
function() ->
    ok.

-spec function2() -> ok.
function2() ->
    ok.

-spec function3(_) -> ok.
function3(_) ->
    ok.