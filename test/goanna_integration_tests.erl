-module(goanna_integration_tests).

%% TODO: change this into common test suite.

-include_lib("eunit/include/eunit.hrl").
-include_lib("goanna.hrl").

-behaviour(goanna_forward_callback_mod).

% TODO: how shall i do this?i've just exported forward/1
% -behaviour("").
-export([
    forward_init/1,
	forward/2,
	trace/1,
	do_trace/1,
	start_dbg/0
]).

%% Find a smarter way of creating a remote node...
api_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"goanna_api_nodes",
            ?_assert(ok==try_test_fun(fun goanna_api_nodes/0))},
        {"goanna_api_add_node",
            ?_assert(ok==try_test_fun(fun goanna_api_add_node/0))},
        {"goanna_api_add_node_cannot_connect",
            ?_assert(ok==try_test_fun(fun goanna_api_add_node_cannot_connect/0))},
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
        %{"reached_max_stop_trace",
        %    ?_assert(ok==try_test_fun(fun reached_max_stop_trace/0))},
        {"list_active_traces",
            ?_assert(ok==try_test_fun(fun list_active_traces/0))}
     ]
    }.

try_test_fun(TestFun) ->
    try
        TestFun(),
        ok
    catch
        C:E ->
            ?debugFmt("test failed: ~p",[{C,E,erlang:get_stacktrace()}]),
            failed
    end.

goanna_api_nodes() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes().

goanna_api_add_node() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,

    %% Adding it
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    % timer:sleep(1),
    % [{Node,Cookie,tcpip_port}] = goanna_api:nodes(),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Adding a duplicate
    {error,{already_started,GoannaNodePid}} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    % timer:sleep(1),
    [{Node,Cookie,tcpip_port}] = goanna_api:nodes().

goanna_api_add_node_cannot_connect() ->
    ok = application:set_env(goanna, max_reconnecion_attempts, 3),

    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    FakeGoannaNode = 'blabla@blahost_blacookie',

    %% Adding it
    {ok, GoannaNodePid} =
        goanna_api:add_node(FakeGoannaNode, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),

    %timer:sleep(1),
    [] = goanna_api:nodes(),

    %% wait for 3*50ms attempts...
    timer:sleep(200),

    %% Node should still not have connected. but hawk know's about it...
    [] = goanna_api:nodes().

goanna_api_add_node_validation() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    %% Invalid data:
     {error, badarg} = goanna_api:add_node(1, 2, 3),
     {error, badarg} = goanna_api:add_node(atom, 2, 3),
     {error, badarg} = goanna_api:add_node(atom, atom, 3),
     {error, badarg} = goanna_api:add_node(atom, atom, fakeone).

remove_node() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,

    %% Try removing a unknown node:
    {error, no_such_node} = goanna_api:remove_node('fake@nohost'),

    %% Add, and then remove a known node:
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    % timer:sleep(1),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)).

remove_node_validation() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {error, badarg} = goanna_api:remove_node(12345),
    {error, badarg} = goanna_api:remove_node("other").

update_default_trace_options() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    timer:sleep(1),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Then get the default values
    undefined = application:get_env(goanna, default_trace_options),
    GoannaState = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=false,
                    trace_time=false } = GoannaState,

    %% Change the default values, Then Check the newly set values
    ok = goanna_api:update_default_trace_options([{time, 1000}]),
    {ok,[{time, 1000}]} = application:get_env(goanna, default_trace_options),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=false,
                    trace_time=1000 } = GoannaState2,

    ok = goanna_api:update_default_trace_options([{messages, 10}]),
    {ok,[{time, 1000}, {messages, 10}]} = application:get_env(goanna, default_trace_options),
    GoannaState3 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=10,
                    trace_time=1000 } = GoannaState3,

    ok = goanna_api:update_default_trace_options([{time, 500}, {messages, 50}]),
    {ok,[{time, 500}, {messages, 50}]} = application:get_env(goanna, default_trace_options),
    GoannaState4 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=50,
                    trace_time=500 } = GoannaState4,

    ok = goanna_api:update_default_trace_options([]),
    {ok,[{time, 500}, {messages, 50}]} = application:get_env(goanna, default_trace_options),
    GoannaState5 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=50,
                    trace_time=500 } = GoannaState5,

    ok = goanna_api:update_default_trace_options([{time, false}, {messages, false}]),
    {ok,[]} = application:get_env(goanna, default_trace_options),
    GoannaState6 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=false,
                    trace_time=false } = GoannaState6.

update_default_trace_options_validation() ->
    %% try setting, when there are no nodes
    ok = goanna_api:update_default_trace_options([]),

    %% try setting, some invalid data
    {error, badarg} = goanna_api:update_default_trace_options(blaaa),
    {error, badarg} = goanna_api:update_default_trace_options(2222),
    {error, badarg} = goanna_api:update_default_trace_options(222.22),
    {error, badarg} = goanna_api:update_default_trace_options({blee, blaa}),

    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Change the default values, Then Check the newly set values
    ok = goanna_api:update_default_trace_options([{time, 1000}]),
    {ok,[{time, 1000}]} = application:get_env(goanna, default_trace_options),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=false,
                    trace_time=1000 } = GoannaState2,

    %% Then set bad values again, and test that nothing changed...
    {error, badarg} = goanna_api:update_default_trace_options(blaaa),
    {error, badarg} = goanna_api:update_default_trace_options(2222),
    {error, badarg} = goanna_api:update_default_trace_options(222.22),
    {error, badarg} = goanna_api:update_default_trace_options({blee, blaa}),

    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=false,
                    trace_time=1000 } = GoannaState2.

set_data_retrival_method() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% Check defaults:
    GoannaState = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState,

    %% THen set the new data retrival method:
    ok = goanna_api:set_data_retrival_method(pull),
    {ok,pull} = application:get_env(goanna, data_retrival_method),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState2,

    %% THen set Push:
    ok = goanna_api:set_data_retrival_method({push, 100, ?MODULE}),
    {ok,{push, 100, ?MODULE}} = application:get_env(goanna, data_retrival_method),
    GoannaState3 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = {push, 100, ?MODULE} } = GoannaState3.


set_data_retrival_method_validation() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
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
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% THen set the new data retrival method:
    ok = goanna_api:set_data_retrival_method(pull),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    ?assertEqual({ok,pull}, application:get_env(goanna, data_retrival_method)),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState2,

    ok = goanna_api:trace(goanna_test_module, function),

    %% Trace some
    ?assert([] == goanna_api:pull_all_traces()),
    ok = goanna_test_module:function(),
    timer:sleep(100), %% Wait at least 50Msec for traces to be stored
    ?assertEqual([], goanna_api:pull_all_traces()).

trace_validation() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    %% just trace some non-sense
    {error, badarg} = goanna_api:trace(1),
    {error, badarg} = goanna_api:trace(1.1),
    {error, badarg} = goanna_api:trace("api", 1),
    {error, badarg} = goanna_api:trace(1, 1),
    {error, badarg} = goanna_api:trace(1, 1, 1),
    {error, badarg} = goanna_api:trace(1, 1, 1, 1),
    {error, badarg} = goanna_api:trace(bla, bla, bla),
    {error, badarg} = goanna_api:trace(bla, bla, bla, bla).

stop_trace() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, tcpip_port),
    ?assert(is_pid(GoannaNodePid)),
    ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

    ok = goanna_api:update_default_trace_options([{time, 1000}]),

    %% Change the default values, Then Check the newly set values
    {ok,[{time, 1000}]} = application:get_env(goanna, default_trace_options),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ trace_msg_total=false,
                    trace_time=1000 } = GoannaState2,

    % ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:set_data_retrival_method(pull),
    rpc:call(Node, goanna_test_module, module_info, []),
    ok = goanna_api:trace(goanna_test_module),
    ?assert([] =/= goanna_api:list_active_traces()),

    timer:sleep(10), %% Wait at least 100Msec for traces to be stored
    %% Trace some, set it for 1s and then stop
    ok = goanna_test_module:function(),
    timer:sleep(100), %% Wait at least 100Msec for traces to be stored
    ?assertEqual([], goanna_api:pull_all_traces()),

    %stop it
    ok = goanna_api:stop_trace(),
    [] = ets:tab2list(tracelist),
    [] = goanna_api:list_active_traces(),
    GoannaState3 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{
        trace_msg_count = 0,
        trace_timer_tref = false,
        trace_active = false
    } = GoannaState3.

% reached_max_stop_trace() ->
%     trace([{goanna_db, [
%                 store,
%                 truncate_tracelist
%            ]},
%            {
%            goanna_node
%            ,
%            [
%                 % disable_all_tracing,
%                 % clear_tracelist_restart_dbg,
%                 % cancel_timer,
%                 handle_call
%                 % reapply_traces
%             ]
%            }
%     ]),
%     %% There should be no nodes, at first.
%     [] = goanna_api:nodes(),
%     {ok, Host} = inet:gethostname(),
%     Node = list_to_atom("tests@"++Host),
%     Cookie = cookie,
%     _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

%     %% Add a node
%     {ok, GoannaNodePid} =
%         goanna_api:add_node(Node, cookie, tcpip_port),
%     ?assert(is_pid(GoannaNodePid)),
%     ?assertEqual(ok, wait_for_node({Node,Cookie,tcpip_port}, 100, 25)),

%     %% Disable based on time (ms)...
%     ok = goanna_api:update_default_trace_options([{time, 100}, {messages,100}]),
%     ok = goanna_api:trace(goanna_test_module, function),
%     %% Conveniently wait for traces to be removed
%     timer:sleep(200),
%     ?assertEqual([], goanna_api:list_active_traces()),

%     %% Disable based on message count...
%     ok = goanna_api:update_default_trace_options([{time, 100000}, {messages,1}]),
%     ok = goanna_api:trace(goanna_test_module, function2),

%     timer:sleep(500),

%     ok = goanna_test_module:function2(),
%     ok = goanna_test_module:function2(),

%     timer:sleep(500),
%     ?assertEqual([], goanna_api:pull_all_traces()),
%     ?assertEqual([], goanna_api:list_active_traces()).

list_active_traces() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
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

setup() ->
    [] = os:cmd("epmd -daemon"),
    timer:sleep(100),
    % ok = application:load(kakapo),
    ok = application:set_env(kakapo, event_handler, []),
    ok = error_logger:tty(false),
    {ok,_} = goanna_api:start(),
    {ok, Host} = inet:gethostname(),
    try_dist(list_to_atom("goanna_eunit_test@"++Host)),

    %% Travis CI errors:
        % *** context setup failed ***
        % **in function slave:start/5 (slave.erl, line 198)
        % in call from goanna_api_tests:setup/0 (/home/travis/build/ruanpienaar/goanna/_build/test/lib/goanna/test/goanna_api_tests.erl, line 464)
        % **exit:not_alive
    {ok, SlaveNodeName} = try_slave(Host),
    % ok = application:load(hawk),
    ok = application:set_env(hawk, conn_retry_wait, 20),
    SlaveNodeName.

try_dist(NodeName) ->
    try_dist(NodeName, 5).

try_dist(_Nodename, X) when X =< 0 ->
    exit(1);
try_dist(NodeName, X) when is_integer(X) ->
    try
        net_kernel:start([NodeName, shortnames])
    catch
        C:E ->
            ?debugFmt("try_dist ~p ~p", [?LINE, {C, E, erlang:get_stacktrace()}]),
            timer:sleep(1000),
            try_dist(NodeName, X-1)
    end.

try_slave(Host) ->
    try_slave(Host, 10).

try_slave(_Host,X) when X =< 0 ->
    exit(1);
try_slave(Host, X) when is_integer(X) ->
    try
        {ok, _} = slave:start(Host, tests)
    catch
        C:E ->
            ?debugFmt("try_slave ~p ~p", [?LINE, {C, E, erlang:get_stacktrace()}]),
            timer:sleep(50),
            try_slave(Host, X-1)
    end.


cleanup(SlaveNodeName) ->
    dbg:stop_clear(),
    [ ok = goanna_api:remove_node(NodeName) || {NodeName,_Cookie,_Type} <- goanna_api:nodes() ],
    [ ok = application:stop(App) ||
        {App,_ErtsVsn,_Vsn}
        <- application:which_applications(), App /= kernel andalso App /= stdlib ],
    ok = slave:stop(SlaveNodeName),
    ok = stop_distrib(),
    ok.



% -spec make_distrib( NodeName::string()|atom(), NodeType::shortnames | longnames) ->
%     ActualNodeName::atom | {error, Reason::term()}.
% make_distrib(NodeName, NodeType) when is_list(NodeName) ->
%     make_distrib(erlang:list_to_atom(NodeName), NodeType);
% make_distrib(NodeName, NodeType) ->
%     case node() of
%         'nonode@nohost' ->
%             [] = os:cmd("epmd -daemon"),
%             case net_kernel:start([NodeName, NodeType]) of
%                 {ok, _Pid} -> node()
%             end;
%         CurrNode ->
%             CurrNode
%     end.

stop_distrib() ->
    ok = net_kernel:stop().

forward_init(_) ->
    ok.

forward(_Node, _TraceMessage) ->
    ok.

    %trace([
    %   {goanna_db}
    %]),

    %trace([
    %   {goanna_db,
    %       [
    %           lookup,
    %           store,
    %           delete_child_id_tracelist,
    %           delete_child_id_trace_pattern
    %       ]
    %   }
    %]),
trace(L) ->
    start_dbg(),
    do_trace(L).

do_trace(L) when is_list(L) ->
    [ do_trace(I) || I <- L ];
do_trace(M) when is_atom(M) ->
    dbg:tpl(M, cx);
do_trace({M, Functions}) when is_list(Functions) ->
    [ do_trace({M, F}) || F <- Functions ];
do_trace({M, Function}) when is_atom(Function) ->
    dbg:tpl(M, Function, cx).

start_dbg() ->
    dbg:tracer(),
    dbg:p(all, call).

wait_for_node(WaitingForNode, _WaitTime, Attempts) when Attempts =< 0 ->
    {error, {node_not_found, WaitingForNode}};
wait_for_node(WaitingForNode, WaitTime, Attempts) ->
    case lists:member(WaitingForNode, goanna_api:nodes()) of
        true ->
            timer:sleep(WaitTime),
            ok;
        false ->
            timer:sleep(WaitTime),
            wait_for_node(WaitingForNode, WaitTime, Attempts-1)
    end.