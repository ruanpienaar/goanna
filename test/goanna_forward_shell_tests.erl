-module(goanna_forward_shell_tests).
-include_lib("eunit/include/eunit.hrl").

forward_init_test() ->
    ?assertEqual(
        ok,
        goanna_forward_shell:forward_init('childId')
    ).

forward_test() ->
    ?assertEqual(
        ok,
        goanna_forward_shell:forward(
            self(),
            {'childId', node, {trace_ts, self(), return_from, bla, {1517,250010,958719}}}
        )
    ).