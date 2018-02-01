-module(goanna_node_sup_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("goanna.hrl").

unit_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [
        fun to_node/0
     ]
    }.

to_node() ->
    ?assertEqual(
        ['node', cookie],
        goanna_node_sup:to_node(list_to_atom("node" ++ ?NODE_COOKIE_SEP ++ "cookie"))
    ).