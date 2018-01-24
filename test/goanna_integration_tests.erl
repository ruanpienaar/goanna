-module(goanna_integration_tests).

%% TODO: change this into common test suite.

-include_lib("eunit/include/eunit.hrl").
-include_lib("goanna.hrl").

-behaviour(goanna_forward_callback_mod).

-define(TEST_NODE_NAME, 'goanna_integration_test_node').

-define(TEST_NODE(Host),
    list_to_atom(atom_to_list(?TEST_NODE_NAME)++"@"++Host)
).

% TODO: how shall i do this?i've just exported forward/1
% -behaviour("").
-export([
    forward_init/1,
    forward/2
]).

%% Find a smarter way of creating a remote node...
api_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"goanna_api_add_node",
            ?_assert(ok==try_test_fun(fun goanna_api_add_node/0))},
        {"goanna_api_add_node_validation",
            ?_assert(ok==try_test_fun(fun goanna_api_add_node_validation/0))},
        {"remove_node",
            ?_assert(ok==try_test_fun(fun remove_node/0))},
        {"remove_node_validation",
            ?_assert(ok==try_test_fun(fun remove_node_validation/0))},
        {"update_default_trace_options",
            ?_assert(ok==try_test_fun(fun update_default_trace_options/0))},
        {"update_default_trace_options_validation",
            ?_assert(ok==try_test_fun(fun update_default_trace_options_validation/0))},
        {"set_data_retrival_method_validation",
            ?_assert(ok==try_test_fun(fun set_data_retrival_method_validation/0))},
        {"set_data_retrival_method",
            ?_assert(ok==try_test_fun(fun set_data_retrival_method/0))},
        {"trace",
            ?_assert(ok==try_test_fun(fun trace/0))},
        {"trace_validation",
            ?_assert(ok==try_test_fun(fun trace_validation/0))},
        {"stop_trace",
            ?_assert(ok==try_test_fun(fun stop_trace/0))},
        {"reached_max_stop_trace time",
           ?_assert(ok==try_test_fun(fun reached_max_stop_trace_time/0))},
        {"reached_max_stop_trace messages",
           ?_assert(ok==try_test_fun(fun reached_max_stop_trace_messages/0))},
        {"list_active_traces",
            ?_assert(ok==try_test_fun(fun list_active_traces/0))}
     ]
    }.

setup() ->
    error_logger:tty(false),
    {ok, Host} = inet:gethostname(),
    {ok, _} = unit_testing:start_distrib(list_to_atom("goanna_eunit_test@"++Host),
        shortnames,
        4000),
    Slaves = unit_testing:slaves_setup([{Host, ?TEST_NODE_NAME}]),
    {ok,_} = goanna_api:start(),
    Slaves.

cleanup(Slaves) ->
    [ ok = goanna_api:remove_node(NodeName) || {NodeName,_Cookie,_Type} <- goanna_api:nodes() ],
    true = unit_testing:wait_for_next_value(10, fun() ->
        goanna_api:nodes() == []
    end, false),
    unit_testing:stop_extra_applications(),
    true = unit_testing:cleanup_slaves(Slaves),
    ok = unit_testing:stop_distrib(),
    timer:sleep(50).

try_test_fun(TestFun) ->
    try
        TestFun(),
        ok
    catch
        C:E ->
            ?debugFmt("test failed: ~p",[{C,E,erlang:get_stacktrace()}]),
            failed
    end.

goanna_api_add_node() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),
    %% Adding a duplicate
    ?assertMatch(
        {ok, GoannaNodePid},
        goanna_api:add_node(Node, Cookie, tcpip_port)
    ),
    ?assertEqual(
        [{Node,Cookie,tcpip_port}],
        goanna_api:nodes()
    ).

goanna_api_add_node_validation() ->
    [] = goanna_api:nodes(),
    %% Invalid data:
    {error, badarg} = goanna_api:add_node(1, 2, 3),
    {error, badarg} = goanna_api:add_node(atom, 2, 3),
    {error, badarg} = goanna_api:add_node(atom, atom, 3),
    {error, badarg} = goanna_api:add_node(atom, atom, fakeone),
    [] = goanna_api:nodes().

remove_node() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    %% Try removing a unknown node:
    {error, no_such_node} = goanna_api:remove_node('fake@nohost'),
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    % timer:sleep(1),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),
    ok = goanna_api:remove_node(Node),
    ?assertEqual(ok, wait_for_removal_of_node({Node,Cookie,tcpip_port}, 100, 25)).

remove_node_validation() ->
    [] = goanna_api:nodes(),
    {error, badarg} = goanna_api:remove_node(12345),
    {error, badarg} = goanna_api:remove_node("other"),
    [] = goanna_api:nodes().

update_default_trace_options() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    timer:sleep(1),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Then get the default values
    undefined = application:get_env(goanna, default_trace_options),
    GoannaState = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ trace_max_msg=false,
    %                 trace_max_time=false } = GoannaState,
    ?assertMatch(
        #{trace_max_msg := false,
          trace_max_time := false},
        GoannaState
    ),

    %% Change the default values, Then Check the newly set values
    ok = goanna_api:update_default_trace_options([{time, 1000}]),
    {ok,[{time, 1000}]} = application:get_env(goanna, default_trace_options),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ trace_max_msg=false,
    %                 trace_max_time=1000 } = GoannaState2,
    #{trace_max_msg := false,
      trace_max_time := 1000} = GoannaState2,

    ok = goanna_api:update_default_trace_options([{messages, 10}]),
    {ok,[{time, 1000}, {messages, 10}]} = application:get_env(goanna, default_trace_options),
    GoannaState3 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ trace_max_msg=10,
    %                 trace_max_time=1000 } = GoannaState3,
    #{trace_max_msg := 10,
      trace_max_time := 1000} = GoannaState3,

    ok = goanna_api:update_default_trace_options([{time, 500}, {messages, 50}]),
    {ok,[{time, 500}, {messages, 50}]} = application:get_env(goanna, default_trace_options),
    GoannaState4 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ trace_max_msg=50,
    %                 trace_max_time=500 } = GoannaState4,
    #{trace_max_msg := 50,
      trace_max_time := 500} = GoannaState4,

    ok = goanna_api:update_default_trace_options([]),
    {ok,[{time, 500}, {messages, 50}]} = application:get_env(goanna, default_trace_options),
    GoannaState5 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ trace_max_msg=50,
    %                 trace_max_time=500 } = GoannaState5,
    #{trace_max_msg := 50,
      trace_max_time := 500} = GoannaState5,

    ok = goanna_api:update_default_trace_options([{time, false}, {messages, false}]),
    {ok,[]} = application:get_env(goanna, default_trace_options),
    GoannaState6 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ trace_max_msg=false,
    %                 trace_max_time=false } = GoannaState6.
    #{trace_max_msg := false,
      trace_max_time := false} = GoannaState6.

update_default_trace_options_validation() ->
    {ok, Host} = inet:gethostname(),
    %% try setting, when there are no nodes
    ok = goanna_api:update_default_trace_options([]),
    %% try setting, some invalid data
    {error, badarg} = goanna_api:update_default_trace_options(blaaa),
    {error, badarg} = goanna_api:update_default_trace_options(2222),
    {error, badarg} = goanna_api:update_default_trace_options(222.22),
    {error, badarg} = goanna_api:update_default_trace_options({blee, blaa}),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Change the default values, Then Check the newly set values
    ok = goanna_api:update_default_trace_options([{time, 1000}]),
    {ok,[{time, 1000}]} = application:get_env(goanna, default_trace_options),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    ?assertMatch(
        #{trace_max_msg := false,
          trace_max_time := 1000},
        GoannaState2
    ),

    %% Then set bad values again, and test that nothing changed...
    {error, badarg} = goanna_api:update_default_trace_options(blaaa),
    {error, badarg} = goanna_api:update_default_trace_options(2222),
    {error, badarg} = goanna_api:update_default_trace_options(222.22),
    {error, badarg} = goanna_api:update_default_trace_options({blee, blaa}),

    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    ?assertMatch(
        #{trace_max_msg := false,
          trace_max_time := 1000},
        GoannaState2
    ).

set_data_retrival_method() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Check defaults:
    GoannaState = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState,
    #{ data_retrival_method := pull } = GoannaState,

    %% THen set the new data retrival method:
    ok = goanna_api:set_data_retrival_method(pull),
    {ok,pull} = application:get_env(goanna, data_retrival_method),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState2,
    #{ data_retrival_method := pull } = GoannaState2,

    %% THen set Push:
    ok = goanna_api:set_data_retrival_method({push, 1000, ?MODULE, 100}),
    {ok,{push, 1000, ?MODULE, 100}} = application:get_env(goanna, data_retrival_method),
    GoannaState3 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ data_retrival_method = {push, 100, ?MODULE} } = GoannaState3.
    #{ data_retrival_method := {push, 1000, ?MODULE, 100} } = GoannaState3.

set_data_retrival_method_validation() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Try and set something invalid:
    {error, badarg} = goanna_api:set_data_retrival_method(1234),
    {error, badarg} = goanna_api:set_data_retrival_method(test),
    {error, badarg} = goanna_api:set_data_retrival_method(push),
    {error, badarg} = goanna_api:set_data_retrival_method({push, bla}).

trace() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% THen set the new data retrival method:
    ok = goanna_api:set_data_retrival_method(pull),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    ?assertEqual({ok,pull}, application:get_env(goanna, data_retrival_method)),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    % #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState2,
    #{ data_retrival_method := pull } = GoannaState2,

    ok = goanna_api:trace(goanna_test_module, function),

    %% Trace some
    ?assert([] == goanna_api:pull_all_traces()),
    ok = goanna_test_module:function(),
    timer:sleep(100), %% Wait at least 50Msec for traces to be stored
    ?assertEqual([], goanna_api:pull_all_traces()).

trace_validation() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% just trace some nonsense
    {error, badarg} = goanna_api:trace(1),
    {error, badarg} = goanna_api:trace(1.1),
    {error, badarg} = goanna_api:trace("api", 1),
    {error, badarg} = goanna_api:trace(1, 1),
    {error, badarg} = goanna_api:trace(1, 1, 1),
    {error, badarg} = goanna_api:trace(bla, bla, bla).

stop_trace() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Make sure we store trace items, for us to pull.
    ok = goanna_api:set_data_retrival_method(pull),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% trace
    ok = goanna_api:trace(goanna_test_module, function),
    ?assertEqual(
        [{{GoannaNode_Cookie,{goanna_test_module, function}},[]}],
        goanna_api:list_active_traces()
    ),

    %% Call the functionality
    %% check that we have trace items
    %% Pull all, should also remove them from the table.
    rpc:call(Node, goanna_test_module, function, []),
    % timer:sleep(500),
    % ?assertMatch(
    %     [{_,
    %         {trace_ts,_,call,
    %             {goanna_test_module,function,[]},
    %             {rpc,_,_},
    %             _
    %         }
    %      },
    %      {_,
    %         {trace_ts,_,return_from,
    %             {goanna_test_module,function,0},
    %             ok,
    %             _
    %         }
    %      }
    %     ],
    %     goanna_api:pull_all_traces()
    % ),

    % %% Stop the traces
    % ?assertEqual([], goanna_api:pull_all_traces()),
    goanna_api:stop_trace(),

    %% call functuanility, and confirm that we do not receive trace items now
    rpc:call(Node, goanna_test_module, function, []),
    timer:sleep(100),
    ?assertEqual([], goanna_api:pull_all_traces()).

reached_max_stop_trace_time() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    ?assertEqual([], goanna_api:list_active_traces()),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Disable based on time (ms)...
    ok = goanna_api:update_default_trace_options([{time, 100}]),
    ok = goanna_api:trace(goanna_test_module, function),
    %% Conveniently wait for more than {time, 100}
    timer:sleep(200),
    ?assertEqual([], goanna_api:list_active_traces()).

reached_max_stop_trace_messages() ->
    {ok, Host} = inet:gethostname(),
    % trace([
    %        %goanna_api,
    %        % goanna_db
    %        % {goanna_db, [
    %        %      store,
    %        %      truncate_tracelist
    %        %  ]},
    %        {goanna_node, [
    %             % disable_all_tracing,
    %             % clear_tracelist_restart_dbg,
    %             % cancel_timer,
    %             handle_call
    %             % reapply_traces
    %         ]}
    % ]),

    [] = application:get_env(goanna, traces, []),

    [] = goanna_api:nodes(),
    ?assertEqual([], goanna_api:list_active_traces()),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    ok = application:set_env(goanna, data_retrival_method, pull),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),
    timer:sleep(100),

    %% Disable based on message count...
    ok = goanna_api:update_default_trace_options([{messages,2}]),
    timer:sleep(100),

    ok = goanna_api:trace(goanna_test_module, function2),
    timer:sleep(100),


    [] = application:get_env(goanna, traces, []),


    ok = goanna_test_module:function2(),
    ok = goanna_test_module:function2(),
    timer:sleep(500),

    ?assertEqual([], goanna_api:pull_all_traces()),
    timer:sleep(100),

    ?assertEqual([], goanna_api:list_active_traces()).

list_active_traces() ->
    {ok, Host} = inet:gethostname(),
    [] = goanna_api:nodes(),
    Node = ?TEST_NODE(Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% just allow the tracing to be set for a long time, to reliably test the below.
    ok = goanna_api:update_default_trace_options([{time, 60000}, {messages,1000000}]),

    %% No traces there, right...
    ?assert(length(goanna_api:list_active_traces()) == 0),

    %% trace something, so we have a tracelist
    ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:trace(goanna_test_module, function2),
    ok = goanna_api:trace(goanna_test_module, function3, 1),

    ?assert(length(ets:tab2list(tracelist)) == 3),
    ?assert(length(goanna_api:list_active_traces()) == 3),

    %% Let's call the same thing again...
    ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:trace(goanna_test_module, function),

    ?assert(length(ets:tab2list(tracelist)) == 3),
    ?assert(length(goanna_api:list_active_traces()) == 3),
    ok = goanna_api:stop_trace(goanna_test_module, function),

    %% Ok, maybe we have to wait for the ets obj to be deleted.
    % timer:sleep(50),

    %% Check that the others survived.
    ?assertEqual(2, length(ets:tab2list(tracelist))),
    ?assertEqual(2, length(goanna_api:list_active_traces())).

%%------------------------------------------------------------------------

forward_init(_) ->
    ok.

forward(_Node, _TraceMessage) ->
    ok.

wait_for_node(WaitingForNode, _WaitTime, Attempts) when Attempts =< 0 ->
    {error, {node_not_found, WaitingForNode}};
wait_for_node(WaitingForNode, WaitTime, Attempts) ->
    case lists:member(WaitingForNode, goanna_api:nodes()) of
        true ->
            ok;
        false ->
            timer:sleep(WaitTime),
            wait_for_node(WaitingForNode, WaitTime, Attempts-1)
    end.

wait_for_removal_of_node(WaitingForNode, _WaitTime, Attempts) when Attempts =< 0 ->
    {error, {node_not_removed, WaitingForNode}};
wait_for_removal_of_node(WaitingForNode, WaitTime, Attempts) ->
    case not lists:member(WaitingForNode, goanna_api:nodes()) of
        true ->
            ok;
        false ->
            timer:sleep(WaitTime),
            wait_for_removal_of_node(WaitingForNode, WaitTime, Attempts-1)
    end.