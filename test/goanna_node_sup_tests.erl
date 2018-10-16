-module(goanna_node_sup_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("goanna.hrl").

% start_link/0 -> goanna_SUITE
% start_child/3 -> goanna_SUITE
% delete_child/1 -> goanna_SUITE

init_test() ->
    ?assertEqual(
        {ok, {{one_for_one, 5, 10}, []}},
        goanna_node_sup:init([])
    ).

id_test() ->
    ?assertEqual(
        'node©cookie',
        goanna_node_sup:id('node', 'cookie')
    ).

to_node_test() ->
    ?assertEqual(
        ['node', cookie],
        goanna_node_sup:to_node(list_to_atom("node" ++ ?NODE_COOKIE_SEP ++ "cookie"))
    ).