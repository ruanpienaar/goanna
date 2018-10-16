-module(goanna_node_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(goanna_forward_callback_mod).
-export([
    forward_init/1,
    forward/2
]).

-include_lib("goanna.hrl").

-spec forward_init(atom()) -> {ok, pid() | atom()} | {error, term()}.
forward_init(_ChildId) ->
    {ok, self()}.

-spec forward(_Process :: pid() | atom(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(_Process, {_Ts, _Node, _TraceItem}) ->
    ok.

goanna_node_unit_test_() ->
    unit_testing:foreach(
        % Setup
        fun() ->
            % ?debugFmt("---------->  SELF SETUP 1 <----------- ~p\n", [self()]),
            ok = application:load(goanna)
        end,
        % Cleanup
        fun(_) ->
            NodeRegName = goanna_node_sup:id('nonode@nohost', cookie),
            case
                unit_testing:wait_for_match(10, fun() ->
                    is_pid(whereis(NodeRegName))
                end, true)
            of
                ok ->
                    Pid = whereis(NodeRegName),
                    true = erlang:unregister(NodeRegName),
                    true = erlang:unlink(Pid),
                    true = erlang:exit(Pid, kill);
                {error, no_answer} ->
                    ok
            end,
            ok = application:unload(goanna)
        end,
        % Tests
        [ {"goanna_node:start_link/0 remote_node_went_down",
            ?_assert(unit_testing:try_test_fun(fun start_link_node_went_down/0))
          },

          {"goanna_node:start_link/0 No existing traces - Pull",
            ?_assert(unit_testing:try_test_fun(fun start_link_no_traces_pull/0))
          },

          {"goanna_node:start_link/0 No existing traces - Push",
            ?_assert(unit_testing:try_test_fun(fun start_link_no_traces_push/0))},

          {"goanna_node:start_link/0 existing traces - Pull",
            ?_assert(unit_testing:try_test_fun(fun start_link_traces_pull/0))
          },

          {"goanna_node:start_link/0 existing traces - Push",
            ?_assert(unit_testing:try_test_fun(fun start_link_traces_push/0))},

          {"goanna_node:start_link/0 forward_mod has only forward - Push",
            ?_assert(unit_testing:try_test_fun(fun start_link_fmhof/0))
          },

          {"goanna_node:stop_all_traces_return_state/1",
            ?_assert(unit_testing:try_test_fun(fun stop_all_traces_return_state/0))
          }
        ],
        % Mocks
        [ % Mocks done in tests  
          {goanna_node_mon, [], [
            {monitor, 
                fun('nonode@nohost' = Node, SelfPid) ->
                    {monitor, Node, SelfPid};
                   ('fake@fake' = Node, SelfPid) ->
                    {monitor, Node, SelfPid}
                end
            }
          ]},
          {goanna_db, [], [
            {init_node, 
                fun(['nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie']) ->
                    {ok, node_obj};
                   (['fake@fake', cookie, tcpip_port, 'fake@fake©cookie']) ->
                    {ok, node_obj}
                end
            },
            {lookup, 
                fun([tracelist, _ChildId]) ->
                    []
                end
            }
          ]}
        ],
        true
    ).

start_link_node_went_down() ->
    ok = application:set_env(goanna, data_retrival_method, pull),
    {ok, Pid} = goanna_node:start_link('fake@fake', cookie, tcpip_port, 'fake@fake©cookie'),
    timer:sleep(100),
    ?assertEqual(
        {current_function,{goanna_node,terminate,0}},
        erlang:process_info(Pid, current_function)
    ),
    ?assertEqual(
        {registered_name,'fake@fake©cookie'},
        erlang:process_info(Pid, registered_name)
    ),
    Pid ! should_have_been_in_terminate_recv_loop,
    timer:sleep(100),
    ?assertEqual(
        undefined,
        erlang:process_info(Pid)
    ).

start_link_no_traces_pull() ->
    ok = unit_testing:mock_expect(goanna_db, lookup, fun([tracelist, 'nonode@nohost©cookie']) -> [] end),
    ok = application:set_env(goanna, data_retrival_method, pull),
    ?assertMatch(
        {ok, _},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ).

start_link_no_traces_push() ->
    ok = unit_testing:mock_expect(goanna_db, lookup, fun([tracelist, 'nonode@nohost©cookie']) -> [] end),
    %% Scenario 1 - Forward module is not compatible
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, some_non_existant_module, 10}),
    ?assertException(
        throw,
        {some_non_existant_module, does_not_exist},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ),
    %% Scenario 2 - Forward module forward_init crashed
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, goanna_forward_crash_mod, 10}),
    ?assertException(
        throw,
        {forward_module, goanna_forward_crash_mod, forward_init_crashed, throw, {crash, on, purpose}},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ),
    %% Scenario 3 - forward module exists, forward_init returns {ok, _}.
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, ?MODULE, 10}),
    ?assertMatch(
        {ok, _},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ).

start_link_traces_pull() ->
    ok = unit_testing:mock_expect(goanna_db, lookup, fun([tracelist, 'nonode@nohost©cookie']) -> [{ets}] end),
    ok = application:set_env(goanna, data_retrival_method, pull),
    ?assertMatch(
        {ok, _},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ).

start_link_traces_push() ->
    ok = unit_testing:mock_expect(goanna_db, lookup, fun([tracelist, 'nonode@nohost©cookie']) -> [{ets}] end),
    %% Scenario 1 - Forward module is not compatible
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, some_non_existant_module, 10}),
    
    ?assertException(
        throw,
        {some_non_existant_module, does_not_exist},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ),
    %% Scenario 2 - Forward module forward_init crashed
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, goanna_forward_crash_mod, 10}),
    ?assertException(
        throw,
        {forward_module, goanna_forward_crash_mod, forward_init_crashed, throw, {crash, on, purpose}},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ),
    %% Scenario 3 - forward module exists, forward_init returns {ok, _}.
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, ?MODULE, 10}),
    ?assertMatch(
        {ok, _},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ).

start_link_fmhof() ->
    ok = application:set_env(goanna, data_retrival_method, {push, 60000, goanna_forward_only, 10}),
    ?assertException(
        throw,
        {goanna_forward_only,not_goanna_forward_module_compatible},
        goanna_node:start_link('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ).

stop_all_traces_return_state() ->
    ok = unit_testing:mock_expect(goanna_db, lookup, fun([nodelist, 'nonode@nohost']) -> 
        [{'nonode@nohost', cookie, tcpip_port}] 
    end),
    ok = unit_testing:mock_expect(goanna_db, truncate_tracelist, fun([]) ->
        true
    end),
    ?assertEqual(
        #{cookie => cookie,
          node => nonode@nohost,
          trace_msg_count => 0,
          trace_timer_tref => undefined,
          tracing => false,
          type => tcpip_port},
        goanna_node:stop_all_traces_return_state(
            #{node => 'nonode@nohost', 
              cookie => cookie, 
              type => tcpip_port, 
              trace_timer_tref => undefined}
        )
    ).

initial_state_test() ->
    ?assertEqual(
        #{ node => 'nonode@nohost',
           cookie => cookie,
           type => tcpip_port,
           child_id => 'nonode@nohost©cookie',
           trace_msg_count => 0,
           trace_timer_tref => undefined,
           trace_client_pid => undefined,
           tracing => false,
           push_timer_tref => undefined,
           previous_trace_client_pid => undefined
        },
        goanna_node:initial_state('nonode@nohost', cookie, tcpip_port, 'nonode@nohost©cookie')
    ).