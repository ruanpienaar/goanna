-module(goanna_api_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("goanna.hrl").

-behaviour(goanna_forward_callback_mod).

% TODO: how shall i do this?i've just exported forward/1
% -behaviour("").
-export([forward/2]).

%% Find a smarter way of creating a remote node...
api_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
          {"API -> nodes/0",
            fun goanna_api_nodes/0}
        , {"API -> add_node/3",
            fun goanna_api_add_node/0}
        , {"API -> add_node_cannot_connect/3",
            fun goanna_api_add_node_cannot_connect/0}
        , {"API -> add_node/3 validation tests",
            fun goanna_api_add_node_validation/0}
        , {"API -> remove_node/1",
            fun remove_node/0}
        , {"API -> remove_node/1 validation tests",
            fun remove_node_validation/0}
        , {"API -> update_default_trace_options/1",
            fun update_default_trace_options/0}
        , {"API -> update_default_trace_options/1 validation tests",
            fun update_default_trace_options_validation/0}
        , {"API -> set_data_retrival_method_validation/1 validation tests",
            fun set_data_retrival_method_validation/0}
        , {"API -> set_data_retrival_method/1 validation tests",
            fun set_data_retrival_method/0}
        , {"API -> trace tests",
            fun trace/0}
        , {"API -> trace_validation tests",
            fun trace_validation/0}
        , {"API -> stop_trace tests",
            fun stop_trace/0}
        , {"API -> reached_max_stop_trace tests",
            fun reached_max_stop_trace/0}
        , {"API -> list_active_traces tests",
            fun list_active_traces/0}
     ]
    }.

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
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% Adding a duplicate
    {error,{already_started,GoannaNodePid}} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% removing it
    ok = goanna_api:remove_node(Node).

goanna_api_add_node_cannot_connect() ->
    ok = application:set_env(goanna, max_reconnecion_attempts, 3),

    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    FakeGoannaNode = 'blabla@blahost_blacookie',

    %% Adding it
    {ok, _GoannaNodePid} =
        goanna_api:add_node(FakeGoannaNode, cookie, erlang_distribution),
    timer:sleep(1),
    [] = goanna_api:nodes(),

    %% wait for 3*50ms attempts...
    timer:sleep(200),

    %% Node should still not have connected. but hawk know's about it...
    [] = goanna_api:nodes(),
    ok = hawk:remove_node(FakeGoannaNode).

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
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),
    ok = goanna_api:remove_node(Node).


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
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

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
                    trace_time=false } = GoannaState6,

    ok = goanna_api:remove_node(Node).

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
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(10),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

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
                    trace_time=1000 } = GoannaState2,

    ok = goanna_api:remove_node(Node).

set_data_retrival_method() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

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
    #?GOANNA_STATE{ data_retrival_method = {push, 100, ?MODULE} } = GoannaState3,

    ok = goanna_api:remove_node(Node).


set_data_retrival_method_validation() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% Try and set something invalid:
    {error, badarg} = goanna_api:set_data_retrival_method(1234),
    {error, badarg} = goanna_api:set_data_retrival_method(test),
    {error, badarg} = goanna_api:set_data_retrival_method(push),
    {error, badarg} = goanna_api:set_data_retrival_method({push, bla}),

    ok = goanna_api:remove_node(Node).

trace() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% THen set the new data retrival method:
    ok = goanna_api:set_data_retrival_method(pull),
    {ok,pull} = application:get_env(goanna, data_retrival_method),
    GoannaState2 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = pull } = GoannaState2,

    ok = goanna_api:trace(goanna_test_module, function),

    %% Trace some
    true = ([] == goanna_api:pull_all_traces()),
    ok = goanna_test_module:function(),
    timer:sleep(50), %% Wait at least 50Msec for traces to be stored
    ?assert([] =/= goanna_api:pull_all_traces()),

    ok = goanna_api:remove_node(Node).

trace_validation() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% just trace some non-sense
    {error, badarg} = goanna_api:trace(1),
    {error, badarg} = goanna_api:trace("api"),
    {error, badarg} = goanna_api:trace(1.1),
    {error, badarg} = goanna_api:trace("api", 1),
    {error, badarg} = goanna_api:trace(1, 1),
    {error, badarg} = goanna_api:trace(1, 1, 1),
    {error, badarg} = goanna_api:trace(1, 1, 1, 1),
    {error, badarg} = goanna_api:trace(bla, bla, bla),
    {error, badarg} = goanna_api:trace(bla, bla, bla, bla),

    ok = goanna_api:remove_node(Node),
    [] = goanna_api:nodes().

stop_trace() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

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
    timer:sleep(10), %% Wait at least 100Msec for traces to be stored
    ?assert([] =/= goanna_api:pull_all_traces()),

    %stop it
    ok = goanna_api:stop_trace(),
    [] = ets:tab2list(tracelist),
    [] = goanna_api:list_active_traces(),
    GoannaState3 = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{
        trace_msg_count = 0,
        trace_timer_tref = false,
        trace_active = false
    } = GoannaState3,

    ok = goanna_api:remove_node(Node).

reached_max_stop_trace() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% Disable based on time (ms)...
    ok = goanna_api:update_default_trace_options([{time, 100}, {messages,100}]),
    ok = goanna_api:trace(goanna_test_module, function),

    %% Conveniently wait
    timer:sleep(125),
    [] = ets:tab2list(tracelist),
    [] = goanna_api:list_active_traces(),

    %% Disable based on message count...
    ok = goanna_api:update_default_trace_options([{time, 100000}, {messages,1}]),
    ok = goanna_api:trace(goanna_test_module, function),

    ok = goanna_test_module:function(),
    % ok = goanna_test_module:function(),

    timer:sleep(10),
    Trcs = goanna_api:pull_all_traces(),
    io:format("~p~n", [length(Trcs)]),
    ?assert([] =/= Trcs),

    timer:sleep(100),
    [] = ets:tab2list(tracelist),
    [] = goanna_api:list_active_traces(),

    ok = goanna_api:remove_node(Node).

list_active_traces() ->
    %% There should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    _GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),

    %% Add a node
    {ok, _GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    timer:sleep(1),
    [{Node,Cookie,erlang_distribution}] = goanna_api:nodes(),

    %% just allow the tracing to be set for a long time, to reliably test the below.
    ok = goanna_api:update_default_trace_options([{time, 60000}, {messages,1000000}]),

    %% No traces there, right...
    ?assert(length(goanna_api:list_active_traces()) == 0),

    %% trace something, so we have a tracelist
    ok = goanna_api:trace(goanna_test_module, function),
    ok = goanna_api:trace(goanna_test_module, function2),
    ok = goanna_api:trace(goanna_test_module, function3, 1),

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
    timer:sleep(50),

    %% Check that the others survived.
    ?assert(length(ets:tab2list(tracelist)) == 2),
    ?assert(length(goanna_api:list_active_traces()) == 2),

    ok = goanna_api:remove_node(Node).

%%------------------------------------------------------------------------

setup() ->
    [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok] =
        goanna_api:start(),
    {ok, Host} = inet:gethostname(),
    make_distrib("tests@"++Host, shortnames),
    slave:start(Host, test1),
    application:set_env(hawk, conn_retry_wait, 20),
    ok.

cleanup(_) ->
    [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok] =
        goanna_api:stop(),
    stop_distrib(),
    ok.

-spec make_distrib( NodeName::string()|atom(), NodeType::shortnames | longnames) ->
    {ok, ActualNodeName::atom} | {error, Reason::term()}.
make_distrib(NodeName, NodeType) when is_list(NodeName) ->
    make_distrib(erlang:list_to_atom(NodeName), NodeType);
make_distrib(NodeName, NodeType) ->
    case node() of
        'nonode@nohost' ->
            [] = os:cmd("epmd -daemon"),
            case net_kernel:start([NodeName, NodeType]) of
                {ok, _Pid} -> node()
            end;
        CurrNode ->
            CurrNode
    end.

stop_distrib()->
    net_kernel:stop().

forward(_Node, _TraceMessage) ->
    ok.