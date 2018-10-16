-module(goanna_app_tests).
-include_lib("eunit/include/eunit.hrl").

goanna_app_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() ->
            ok = application:load(goanna),
            % Run the unit test for 2 nodes
            ok = application:set_env(goanna, nodes, [
                {node1, cookie1},
                {node2, cookie2}
            ]),
            % add one of each of the trace items
            ok = application:set_env(goanna, traces, [
                {module},
                {module, func},
                {module, func, arr},
                "ets:lookup(Cust, customers) -> return"
            ])
        end,
        % Cleanup
        fun(_) ->
            ok = application:unload(goanna)
        end,
        % Tests
        [
          % Example test
          {"goanna_app:start/2",
               ?_assert(unit_testing:try_test_fun(fun start/0))},
          {"goanna_app:stop/0",
               ?_assert(unit_testing:try_test_fun(fun stop/0))}
        ],
        % Mocks started at setup phase
        [
            {goanna_sup,
                [],
                [{start_link, 0, {ok, pid}}]
            },
            {goanna_db,
                [],
                [{init, 0, ok},
                 {store_trace_pattern,
                    fun
                        (_ChildId, [{module}], []) ->
                            true;
                        (_ChildId, [{module, func}], []) ->
                            true;
                        (_ChildId, [{module, func, arr}], []) ->
                            true;
                        (_ChildId, [{ets,lookup,2,
                                    [{['$1',customers],[],[{exception_trace}]}]}], []) ->
                            true
                    end}
                ]
            },
            {goanna_api,
                [],
                [{add_node,
                    fun
                        (node1, cookie1) ->
                            {ok, pid};
                        (node2, cookie2) ->
                            {ok, pid}
                    end}
                ]
            }
        ],
        % Strict meck unload
        true
    ).

start() ->
    ?assertEqual(
        {ok, pid},
        goanna_app:start(normal, normal)
    ),
    unit_testing:mock_expect(goanna_sup, start_link, 0, failure_response),
    ?assertEqual(
        failure_response,
        goanna_app:start(normal, normal)
    ).

stop() ->
    ?assertEqual(
        ok,
        goanna_app:stop(state)
    ).