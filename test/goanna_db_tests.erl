-module(goanna_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("goanna.hrl").

goanna_db_test_() ->
    {setup,
     fun() ->
        goanna_db:init()
     end,
     fun(_) ->
        true = ets:delete(nodelist),
        true = ets:delete(tracelist),
        true = ets:delete(relay_tcpip_allocated_ports)
     end,
     [
        fun delete_child_id_tracelist/0
     ]
    }.

% init() ->
% init_node([Node, Cookie, Type]) ->
% nodes() ->
% store(ChildId, Trace) when is_atom(ChildId) ->
% lookup([nodelist, Node]) ->
% all(tracelist) ->
% delete_node(Node, Cookie) ->
delete_child_id_tracelist() ->
    Node = nonode@nohost,
    Node2 = nonode2@nohost,
    Cookie = somecookie,
    ChildId = goanna_node_sup:id(Node, Cookie),
    ChildId2 = goanna_node_sup:id(Node2, Cookie),
    TrcPattern = #trc_pattern{m=goanna_test_module, f=function},
    TrcPattern2 = #trc_pattern{m=goanna_test_module, f=function2},
    Opts = [{messages,1},{time,100000}],
    true = goanna_db:store([tracelist, ChildId, TrcPattern], Opts),
    ?assertMatch( %% 1 trc pattern 1 child
        [ {
           {_,{trc_pattern,goanna_test_module,function,undefined,undefined}},
           [{messages,1},{time,100000}]
          }
        ],
        goanna_db:all(tracelist)
    ),
    true = goanna_db:store([tracelist, ChildId, TrcPattern2], Opts),
    ?assertMatch( %% 2 trc pattern 1 child
        [ {
           {ChildId,{trc_pattern,goanna_test_module,function,undefined,undefined}},
           [{messages,1},{time,100000}]
          },
          {
           {ChildId,{trc_pattern,goanna_test_module,function2,undefined,undefined}},
           [{messages,1},{time,100000}]
          }
        ],
        lists:sort(goanna_db:all(tracelist))
    ),
    true = goanna_db:store([tracelist, ChildId2, TrcPattern], Opts),
    ?assertMatch( %% 3 trc pattern 2 children
        [ {
           {ChildId2,{trc_pattern,goanna_test_module,function,undefined,undefined}},
           [{messages,1},{time,100000}]
          },
          {
           {ChildId,{trc_pattern,goanna_test_module,function,undefined,undefined}},
           [{messages,1},{time,100000}]
          },
          {
           {ChildId,{trc_pattern,goanna_test_module,function2,undefined,undefined}},
           [{messages,1},{time,100000}]
          }
        ],
        lists:sort(goanna_db:all(tracelist))
    ),

    true = goanna_db:delete_child_id_tracelist(Node, Cookie),
    ?assertMatch( %% 1 trc pattern 1 child, other 2 are deleted
        [ {
           {ChildId2,{trc_pattern,goanna_test_module,function,undefined,undefined}},
           [{messages,1},{time,100000}]
          }
        ],
        goanna_db:all(tracelist)
    ),
    true = goanna_db:delete_child_id_tracelist(Node2, Cookie),
    ?assertMatch( %% child2 trc pattern also deleted
        [],
        goanna_db:all(tracelist)
    ).

% delete_child_id_trace_pattern(Node, Cookie, TrcPattern) ->
% truncate_tracelist([]) ->
% truncate_traces(Tbl) ->
% first(Tbl) ->
% next(Tbl, Continuation) ->
% end_of_table() ->
% lookup_entry(Tbl, Key) ->
% delete(Tbl, Key) ->
% pull(Tbl) ->
% pull(Tbl, BatchSize) ->
