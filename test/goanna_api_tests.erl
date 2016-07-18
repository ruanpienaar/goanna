-module(goanna_api_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("goanna.hrl").

% TODO: how shall i do this?i've just exported forward/1
% -behaviour("").
-export([forward/1]).

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
        , {"API -> set_data_retrival_method/1 validation tests",
            fun set_data_retrival_method_validation/0}
        , {"API -> set_data_retrival_method/1 validation tests",
            fun set_data_retrival_method_validation/0}
     ]
    }.

goanna_api_nodes() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes().

goanna_api_add_node() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Adding it
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% Adding a duplicate
    {error,{already_started,GoannaNodePid}} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% removing it
    ok = goanna_api:remove_node(Node).

goanna_api_add_node_validation() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    %% Invalid data:
     {error, badarg} = goanna_api:add_node(1, 2, 3),
     {error, badarg} = goanna_api:add_node(atom, 2, 3),
     {error, badarg} = goanna_api:add_node(atom, atom, 3),
     {error, badarg} = goanna_api:add_node(atom, atom, fakeone).

remove_node() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Try removing a unknown node:
    {error, no_such_node} = goanna_api:remove_node('fake@nohost'),

    %% Add, and then remove a known node:
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),
    ok = goanna_api:remove_node(Node).


remove_node_validation() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {error, badarg} = goanna_api:remove_node(12345),
    {error, badarg} = goanna_api:remove_node("other").

update_default_trace_options() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

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

    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

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
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% Check defaults:
    {ok,pull} = application:get_env(goanna, data_retrival_method),
    GoannaState = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = pull,
                    forward_callback_mod = undefined } = GoannaState,

    %% THen set the new data retrival method:
    ok = goanna_api:set_data_retrival_method(pull),
    {ok,pull} = application:get_env(goanna, data_retrival_method),
    GoannaState = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = pull,
                    forward_callback_mod = undefined } = GoannaState,

    %% THen set Push:
    ok = goanna_api:set_data_retrival_method({push, 100, ?MODULE}),
    {ok,{push, 100, ?MODULE}} = application:get_env(goanna, data_retrival_method),
    GoannaState = sys:get_state(GoannaNode_Cookie),
    #?GOANNA_STATE{ data_retrival_method = {push, 100, ?MODULE},
                    forward_callback_mod = ?MODULE } = GoannaState,

    ok = goanna_api:remove_node(Node).


set_data_retrival_method_validation() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Add a node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% Try and set something invalid:
    {error, badarg} = goanna_api:set_data_retrival_method(1234),
    {error, badarg} = goanna_api:set_data_retrival_method(test),
    {error, badarg} = goanna_api:set_data_retrival_method(push),
    {error, badarg} = goanna_api:set_data_retrival_method({push, bla}),

    ok = goanna_api:remove_node(Node).

%%------------------------------------------------------------------------

setup() ->
    [_,_,_,_,_,_,_,_,_,_,_] =
        goanna_api:start(),
    {ok, Host} = inet:gethostname(),
    make_distrib("tests@"++Host, shortnames),
    slave:start(Host, test1),
    ok.

cleanup(_) ->
    [_,_,_,_,_,_,_,_,_,_,_] =
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

forward(TraceMessage) ->
    ok.