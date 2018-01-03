-module(goanna_db_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("goanna.hrl").

-export([
    forward/2
]).

goanna_db_test_() ->
    {foreach,
     fun() ->
        ?assertEqual(
            relay_tcpip_allocated_ports,
            goanna_db:init()
        ),
        ?assertMatch(
               [{read_concurrency,false},
                {write_concurrency,false},
                {compressed,false},
                {memory,_},
                {owner,_},
                {heir,none},
                {name,nodelist},
                {size,0},
                {node,_},
                {named_table,true},
                {type,set},
                {keypos,1},
                {protection,public}],
            ets:info(nodelist)
        ),
        ?assertMatch(
               [{read_concurrency,false},
                {write_concurrency,false},
                {compressed,false},
                {memory,_},
                {owner,_},
                {heir,none},
                {name,tracelist},
                {size,0},
                {node,_},
                {named_table,true},
                {type,set},
                {keypos,1},
                {protection,public}],
            ets:info(tracelist)
        ),
        ?assertMatch(
               [{read_concurrency,false},
                {write_concurrency,false},
                {compressed,false},
                {memory,_},
                {owner,_},
                {heir,none},
                {name,relay_tcpip_allocated_ports},
                {size,0},
                {node,_},
                {named_table,true},
                {type,set},
                {keypos,1},
                {protection,public}],
            ets:info(relay_tcpip_allocated_ports)
        ),
        ok
     end,
     fun(_) ->
        true = ets:delete(nodelist),
        true = ets:delete(tracelist),
        true = ets:delete(relay_tcpip_allocated_ports)
     end,
     [
        fun init_node/0,
        fun nodes/0,
        fun store_trace/0,
        fun store_trace_pattern/0,
        fun lookup/0,
        fun all/0,
        fun delete_node/0,
        fun delete_child_id_tracelist/0,
        fun delete_child_id_trace_pattern/0,
        fun truncate_tracelist/0,
        fun truncate_traces/0,
        fun pull1/0,
        fun pull2/0,
        fun pull4/0,
        fun push/0
     ]
    }.

init_node() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % - Type = file
    Node2 = 'another@localhost',
    Cookie2 = 'chocchip',
    Type2 = file,
    ChildId2 = goanna_node_sup:id(Node2, Cookie2),
    ?assertEqual(
        {ok, {Node2, Cookie2, Type2}},
        goanna_db:init_node([Node2, Cookie2, Type2, ChildId2])
    ).

nodes() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % - Type = file
    Node2 = 'another@localhost',
    Cookie2 = 'chocchip',
    Type2 = file,
    ChildId2 = goanna_node_sup:id(Node2, Cookie2),
    ?assertEqual(
        {ok, {Node2, Cookie2, Type2}},
        goanna_db:init_node([Node2, Cookie2, Type2, ChildId2])
    ),

    ?assertEqual(
        [{Node2,Cookie2,Type2},
         {Node1,Cookie1,Type1}],
        goanna_db:nodes()
    ).

store_trace() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % Store the trace item
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label, info, {1515,12494,391750}}
        )
    ),

    % Store the trace item
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label, info, extra, {1515,12494,391750}}
        )
    ).

store_trace_pattern() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    TrcPattern = [{trace, [{m, f, 2}]}],
    Opts = [],
    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern, Opts)
    ).

lookup() ->
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    TrcPattern1 = [{trace, [{m, f, 2}]}],
    TrcPattern2 = [{trace, [{m2, f2, 2}]}],

    %% Empty test
    ?assertEqual(
        [],
        goanna_db:lookup([nodelist, Node1])
    ),
    ?assertEqual(
        [],
        goanna_db:lookup([tracelist, ChildId1])
    ),
    ?assertEqual(
        [],
        goanna_db:lookup([tracelist, ChildId1, TrcPattern1])
    ),
    ?assertEqual(
        [],
        goanna_db:lookup([tracelist, Node1, Cookie1, TrcPattern1])
    ),

    %% Set some values
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern1, [])
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern2, [])
    ),

    %% Values test
    ?assertEqual(
        [{Node1, Cookie1, Type1}],
        goanna_db:lookup([nodelist, Node1])
    ),
    ?assertEqual(
        [{{ChildId1, TrcPattern1}, []},
         {{ChildId1, TrcPattern2}, []}],
        lists:sort(
            goanna_db:lookup([tracelist, ChildId1])
        )
    ),
    ?assertEqual(
        [{{ChildId1, TrcPattern1}, []}],
        goanna_db:lookup([tracelist, ChildId1, TrcPattern1])
    ),
    ?assertEqual(
        [{{ChildId1, TrcPattern2}, []}],
        goanna_db:lookup([tracelist, ChildId1, TrcPattern2])
    ),
    ?assertEqual(
        [{{ChildId1, TrcPattern1}, []}],
        goanna_db:lookup([tracelist, Node1, Cookie1, TrcPattern1])
    ).

all() ->
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    TrcPattern1 = [{trace, [{m, f, 2}]}],
    TrcPattern2 = [{trace, [{m2, f2, 2}]}],

    %% Empty tests
    ?assertEqual(
        [],
        goanna_db:all(tracelist)
    ),
    ?assertEqual(
        [],
        goanna_db:all(nodelist)
    ),

    %% Set some values
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern1, [])
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern2, [])
    ),

    %% Values test
    ?assertEqual(
        [{{ChildId1, TrcPattern1}, []},
         {{ChildId1, TrcPattern2}, []}],
        lists:sort(
            goanna_db:all(tracelist)
        )
    ),
    ?assertEqual(
        [{test@localhost,oreo,tcpip_port}],
        lists:sort(
            goanna_db:all(nodelist)
        )
    ),

    ?assertEqual(
        [],
        goanna_db:all(bla)
    ).

delete_node() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),
    ?assertEqual(
        true,
        goanna_db:delete_node(Node1, Cookie1)
    ).

delete_child_id_tracelist() ->
    Node = nonode@nohost,
    Node2 = nonode2@nohost,
    Cookie = somecookie,
    ChildId1 = goanna_node_sup:id(Node, Cookie),
    ChildId2 = goanna_node_sup:id(Node2, Cookie),
    %TrcPattern = #trc_pattern{m=goanna_test_module, f=function},
    TrcPattern = {goanna_test_module, function},
    % TrcPattern2 = #trc_pattern{m=goanna_test_module, f=function2},
    TrcPattern2 = {goanna_test_module, function2},
    Opts = [{messages,1},{time,100000}],
    true = goanna_db:store_trace_pattern(ChildId1, TrcPattern, Opts),
    ?assertEqual( %% 1 trc pattern 1 child
        [{{'nonode@nohost©somecookie',{goanna_test_module,function}},
            Opts}],
        goanna_db:all(tracelist)
    ),
    true = goanna_db:store_trace_pattern(ChildId1, TrcPattern2, Opts),
    ?assertMatch( %% 2 trc pattern 1 child
        [ {{ChildId1,TrcPattern}, Opts},
          {{ChildId1,TrcPattern2}, Opts}
        ],
        lists:sort(goanna_db:all(tracelist))
    ),
    true = goanna_db:store_trace_pattern(ChildId2, TrcPattern, Opts),
    ?assertMatch( %% 3 trc pattern 2 children
        [ {{ChildId2,TrcPattern}, Opts},
          {{ChildId1,TrcPattern}, Opts},
          {{ChildId1,TrcPattern2}, Opts}
        ],
        lists:sort(goanna_db:all(tracelist))
    ),

    true = goanna_db:delete_child_id_tracelist(Node, Cookie),
    ?assertMatch( %% 1 trc pattern 1 child, other 2 are deleted
        [ {{ChildId2,TrcPattern}, Opts}
        ],
        goanna_db:all(tracelist)
    ),
    true = goanna_db:delete_child_id_tracelist(Node2, Cookie),
    ?assertMatch( %% child2 trc pattern also deleted
        [],
        goanna_db:all(tracelist)
    ).

delete_child_id_trace_pattern() ->
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    TrcPattern1 = [{trace, [{m, f, 2}]}],
    ?assertEqual(
        true,
        goanna_db:delete_child_id_trace_pattern(Node1, Cookie1, TrcPattern1)
    ),

    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern1, [])
    ),
    ?assertEqual(
        [{{ChildId1, TrcPattern1}, []}],
        goanna_db:lookup([tracelist, ChildId1, TrcPattern1])
    ),

    ?assertEqual(
        true,
        goanna_db:delete_child_id_trace_pattern(Node1, Cookie1, TrcPattern1)
    ),
    ?assertEqual(
        [],
        goanna_db:lookup([tracelist, ChildId1])
    ).

truncate_tracelist() ->
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    TrcPattern1 = [{trace, [{m, f, 2}]}],
    TrcPattern2 = [{trace, [{m2, f2, 2}]}],

    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern1, [])
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace_pattern(ChildId1, TrcPattern2, [])
    ),
    %% Values test
    ?assertEqual(
        [{{ChildId1, TrcPattern1}, []},
         {{ChildId1, TrcPattern2}, []}],
        lists:sort(
            goanna_db:all(tracelist)
        )
    ),

    %% Truncate trace patterns
    ?assertEqual(
        true,
        goanna_db:truncate_tracelist([])
    ),
    ?assertEqual(
        [],
        goanna_db:all(tracelist)
    ).

truncate_traces() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % Store the trace item
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label, info, {1515,12494,391750}}
        )
    ),

    % Truncate traces
    ?assert(
        [] /= ets:tab2list(ChildId1)
    ),
    ?assertEqual(
        true,
        goanna_db:truncate_traces(ChildId1)
    ),
    ?assert(
        [] == ets:tab2list(ChildId1)
    ).

pull1() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % Store the trace item
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label, info, {1515,12494,391750}}
        )
    ),

    ?assertEqual(
        [{{1515,12494,391750}, {trace_ts,self(),label,info,{1515,12494,391750}}}],
        goanna_db:pull(ChildId1)
    ),

    ?assertEqual(
        [],
        goanna_db:pull(ChildId1)
    ).

pull2() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % Store the trace item
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label, info, {1515,12494,391750}}
        )
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label2, info2, {1516,12494,391750}}
        )
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label3, info3, {1517,12494,391750}}
        )
    ),

    ?assertEqual(
        [{{1515,12494,391750},{trace_ts,self(),label,info,{1515,12494,391750}}}],
        goanna_db:pull(ChildId1, 1)
    ),
    ?assertEqual(
        [{{1516,12494,391750},{trace_ts,self(),label2,info2,{1516,12494,391750}}},
         {{1517,12494,391750},{trace_ts,self(),label3,info3,{1517,12494,391750}}}],
        goanna_db:pull(ChildId1, 2)
    ),

    ?assertEqual(
        [],
        goanna_db:pull(ChildId1, 1)
    ).

pull4() ->
    ok.

push() ->
    % - Type = tcpip_port
    Node1 = 'test@localhost',
    Cookie1 = 'oreo',
    Type1 = tcpip_port,
    ChildId1 = goanna_node_sup:id(Node1, Cookie1),
    ?assertEqual(
        {ok, {Node1, Cookie1, Type1}},
        goanna_db:init_node([Node1, Cookie1, Type1, ChildId1])
    ),

    % Store the trace item
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label, info, {1515,12494,391750}}
        )
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label2, info2, {1516,12494,391750}}
        )
    ),
    ?assertEqual(
        true,
        goanna_db:store_trace(
            ChildId1,
            {trace_ts, self(), label3, info3, {1517,12494,391750}}
        )
    ),

    ?assertEqual(
        ok,
        goanna_db:push(ChildId1, ?MODULE, 1)
    ),

    ?assertEqual(
        [{{1516,12494,391750},{trace_ts,self(),label2,info2,{1516,12494,391750}}},
         {{1517,12494,391750},{trace_ts,self(),label3,info3,{1517,12494,391750}}}],
        goanna_db:pull(ChildId1, 2)
    ),

    ?assertEqual(
        ok,
        goanna_db:push(ChildId1, ?MODULE, 2)
    ),
    ?assert(
        [] == ets:tab2list(ChildId1)
    ).

%% Internal Eunit

forward(_ChildId, _Entry) ->
    ok.

%% Proper Tests

prop_goanna_db_all() ->
    ?FORALL(
        R,
        atom(),
        valid_response(goanna_db:all(R))
    ).

valid_response({error, unknown_table}) ->
    true;
valid_response(R) when is_list(R) ->
    true;
valid_response(_) ->
    false.

prop_init_node() ->
    % init goanna_db
    goanna_db:init(),
    ?FORALL(
        Node,
        atom(),
        ?IMPLIES(valid_node(Node), setup(Node, {key, value}))
    ).

setup(Node, Trace) ->
    % init the node,
    Cookie = oreo,
    ChildId = goanna_node_sup:id(Node, Cookie),
    {ok, _} = goanna_db:init_node([Node, Cookie, tcpip_port, ChildId]),
    % io:format("NODE ~p SIZE ... ~p~n", [Node, ets:info(ChildId, size)]),
    % then store a trace pattern.
    A = goanna_db:store_trace_pattern(ChildId, Trace, []),
    ets:delete(ChildId),
    A.

valid_node(N) when N == '' orelse
                   N == '?' ->
    false;
valid_node(_N) ->
    true.