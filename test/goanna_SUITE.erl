-module(goanna_SUITE).

-export([
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    group/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).
% Success Tests
-export([
    add_node/1,
    add_node_callbacks/1,
    remove_node/1,
    remove_goanna_node/1,
    remove_goanna_callbacks/1,
    update_default_trace_options/1,
    set_data_retrival_method/1,
    trace/1,
    trace_ms/1,
    trace_modules/1,
    stop_trace/1,
    clear_all_traces/1,
    list_active_traces/1,
    pull_all_traces/1

]).
% Failure tests
-export([

]).
-include_lib("common_test/include/ct.hrl").

% Returns a list of all test cases and groups in the suite. (Mandatory)
all() ->
    [{group, success_test_group}
    %,{group, failure_test_group}
    ].

all_success() ->
    [
        add_node,
        add_node_callbacks,
        remove_node,
        remove_goanna_node,
        remove_goanna_callbacks,
        update_default_trace_options,
        set_data_retrival_method,
        trace,
        trace_ms,
        trace_modules,
        stop_trace,
        clear_all_traces,
        list_active_traces,
        pull_all_traces
    ].

% all_failure() ->
%     [

%     ].

% unit_testing:try_test_fun(fun goanna_api_add_node/0))},
% unit_testing:try_test_fun(fun goanna_api_add_node_validation/0))},
% unit_testing:try_test_fun(fun remove_node/0))},
% unit_testing:try_test_fun(fun remove_node_validation/0))},
% unit_testing:try_test_fun(fun update_default_trace_options/0))},
% unit_testing:try_test_fun(fun update_default_trace_options_validation/0))},
% unit_testing:try_test_fun(fun set_data_retrival_method_validation/0))},
% unit_testing:try_test_fun(fun set_data_retrival_method/0))},
% unit_testing:try_test_fun(fun trace/0))},
% unit_testing:try_test_fun(fun trace_validation/0))},
% unit_testing:try_test_fun(fun stop_trace/0))},
% unit_testing:try_test_fun(fun reached_max_stop_trace_time/0))},
% unit_testing:try_test_fun(fun reached_max_stop_trace_messages/0))},
% unit_testing:try_test_fun(fun list_active_traces/0))}

% Information function used to return properties for the suite. (Optional)
suite() ->
    [{timetrap, {minutes, 10}} % wait for 10, better than the default 30min wait.
    ].

% For declaring test case groups. (Optional)
groups() ->
    [
        {success_test_group, [shuffle,{repeat,10}], all_success()}
     %  ,{failure_test_group, [shuffle,{repeat,10}], all_failure()}
    ].

% Suite level configuration function, executed before the first test case. (Optional)
init_per_suite(Config) ->
    [] = os:cmd("epmd -daemon"), % for travis ci
    {ok, _} = erlang_testing:start_distrib(new_node_name(), shortnames),
    ok = application:load(goanna),
    ok = application:set_env(goanna, default_trace_options, []),
    {ok, DepApps} = application:ensure_all_started(goanna),
    [{dep_apps, DepApps}|Config].

% Suite level configuration function, executed after the last test case. (Optional)
end_per_suite(Config) ->
    ct:log("~p~n", [application:which_applications()]),
    ok = application:stop(goanna),
    {dep_apps, DepApps} = lists:keyfind(dep_apps, 1, Config),
    [ ok = application:stop(D) || D <- DepApps, D =/= goanna ],
    erlang_testing:stop_distrib().

% Information function used to return properties for a test case group. (Optional)
group(_GroupName) ->
    [].

% Configuration function for a group, executed before the first test case. (Optional)
init_per_group(_GroupName, Config) ->
    Config.

% Configuration function for a group, executed after the last test case. (Optional)
end_per_group(_GroupName, _Config) ->
    ok.

% Configuration function for a testcase, executed before each test case. (Optional)
init_per_testcase(TestCase, Config) ->

    case TestCase of
        update_default_trace_options ->
            % {ok, _} = dbg:tracer(),
            % {ok, _} = dbg:p(all, call),
            % {ok, _} = dbg:tpl(goanna_api, cx);
            ok;
        _ ->
            ok
    end,

    ok = application:set_env(hawk, connection_retries, 600),
    ok = application:set_env(hawk, conn_retry_wait, 100),
    node_table = ets:new(node_table, [public, named_table, set]),
    {ok, Host} = inet:gethostname(),
    N1 = new_node_name(),
    % N2 = new_node_name(),
    % N3 = new_node_name(),
    % N4 = new_node_name(),
    % N5 = new_node_name(),
    Slaves = erlang_testing:slaves_setup([
        {Host, N1}
       % ,{Host, N2}
       % ,{Host, N3}
       % ,{Host, N4}
       % ,{Host, N5}
    ]),
    ct:log("init_per_testcase Slaves -> ~p~n", [Slaves]),
    [{slaves, Slaves} | Config].

% Configuration function for a testcase, executed after each test case. (Optional)
end_per_testcase(_TestCase, Config) ->
    ok = dbg:stop_clear(),
    true = ets:delete(node_table),
    ok = lists:foreach(fun({Node, _Cookie,_T}) ->
        goanna_api:remove_node(Node),
        goanna_api:remove_goanna_node(Node)
    end, goanna_api:nodes()),
    ok = lists:foreach(fun(HN) ->
        ok = hawk:remove_node(HN)
    end, hawk:nodes()),
    F = fun() ->
        goanna_api:nodes() == []
    end,
    unit_testing:wait_for_match(100, F, true),
    {slaves, Slaves} = lists:keyfind(slaves, 1, Config),
    true = erlang_testing:cleanup_slaves(Slaves).

% The test case function.
add_node(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]).

add_node_callbacks(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]),
    % Add goanna node callbacks
    ok = goanna_api:add_node_callbacks(Node, Cookie, tcpip_port).

remove_node(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]),

    % Remove the hawk node - which will remove the goanna node
    ok = goanna_api:remove_node(Node),
    F2 = fun() ->
        lists:member(Node, goanna_api:nodes())
    end,
    unit_testing:wait_for_match(100, F2, false).

remove_goanna_node(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]),

    % Remove goanna node only
    ok = goanna_api:remove_goanna_node(Node),
    F2 = fun() ->
        lists:member(Node, goanna_api:nodes())
    end,
    unit_testing:wait_for_match(100, F2, false).

remove_goanna_callbacks(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]),

    % remove goanna callbacks from hawk
    true = goanna_api:remove_goanna_callbacks(Node),
    F2 = fun() ->
        case hawk:node_exists(Node) of
            {ok, _, []} ->
                true;
            _ ->
                false
        end
    end,
    unit_testing:wait_for_match(100, F2, false).


update_default_trace_options(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]),
    ct:pal("1"),
    GoannaNode_Cookie = goanna_node_sup:id(Node,Cookie),
    ct:pal("2"),
    %% Then get the default values
    {ok, []} = application:get_env(goanna, default_trace_options),
    ct:pal("3"),

    GoannaState = sys:get_state(GoannaNode_Cookie),

    ct:pal("4"),

    #{trace_max_msg := false,
      trace_max_time := false} = GoannaState,

    ct:pal("5"),

    % #?GOANNA_STATE{ trace_max_msg=false,
    %                 trace_max_time=false } = GoannaState,
    % ?assertMatch(
    %     #{trace_max_msg := false,
    %       trace_max_time := false},
    %     GoannaState
    % ),

    %% Change the default values, Then Check the newly set values
    ok = goanna_api:update_default_trace_options([{time, 1000}]),

    ct:pal("6"),

    % Keep calling to check if the call was made
    {ok,[{time, 1000}]} = application:get_env(goanna, default_trace_options),

    ct:pal("7"),

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

set_data_retrival_method(_Config) ->
    ok.

trace(Config) ->
    {slaves, [Slave|_]} = lists:keyfind(slaves, 1, Config),
    Cookie = erlang:get_cookie(),
    Node = Slave,
    [] = goanna_api:nodes(),
    % Add node
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, Cookie, tcpip_port),
    true = is_pid(GoannaNodePid),
    F = fun() ->
        goanna_api:nodes()
    end,
    % Check that node is added
    unit_testing:wait_for_match(100, F, [{Node,Cookie,tcpip_port}]),


    % trace/1
    ok = goanna_api:trace(goanna_test_module),
    % trace/2
    % trace/3

    [ ok = goanna_test_module:function() || _ <- lists:seq(1, 100) ],

    ok.

trace_ms(_Config) ->
    ok.

trace_modules(_Config) ->
    ok.

stop_trace(_Config) ->
    ok.

clear_all_traces(_Config) ->
    ok.

list_active_traces(_Config) ->
    ok.

pull_all_traces(_Config) ->
    ok.


%% ------
new_node_name() ->
    list_to_atom(erlang:ref_to_list(make_ref()) -- "#Ref<>...").