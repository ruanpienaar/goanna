-module (goanna_db).
-export ([
          init/0,
          init_node/1,
          node_table_exists/2,
          store/2,
          lookup/1,
          delete_node/1,
          delete_tracelist_pattern/2,
          truncate_tracelist/1
]).

%% API

init() ->
    nodelist = ets:new(nodelist, [public, set, named_table]),
    tracelist = ets:new(tracelist, [public, set, named_table]),
    relay_tcpip_allocated_ports = ets:new(relay_tcpip_allocated_ports, [public, set, named_table]).

init_node([Node, Cookie, Type]) ->
    NodeObj = {Node, Cookie, Type},
    true = ets:insert(nodelist, NodeObj),
    ChildId = goanna_node_sup:id(Node, Cookie),
    ChildId = ets:new(ChildId, [public, ordered_set, named_table]),
    {ok, NodeObj}.

node_table_exists(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    case ets:info(ChildId, size) of
        undefined            -> false;
        S when is_integer(S) -> true
    end.

store([trace, ChildId], Trace) ->
    ets:insert(ChildId, {erlang:timestamp(),Trace});
store([trace, Node, Cookie], Trace) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:insert(ChildId, {erlang:timestamp(),Trace});
store([tracelist, ChildId], TracePattern) ->
    ets:insert(tracelist, {{ChildId, TracePattern}, erlang:timestamp()}).

lookup([nodelist, Node]) ->
    ets:lookup(nodelist, Node);
lookup([trc_pattern, ChildId, TracePattern]) ->
    ets:lookup(tracelist, {ChildId, TracePattern});
lookup([trc_pattern, Node, Cookie, TracePattern]) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:lookup(tracelist, {ChildId, TracePattern});
lookup([trace, Tbl, Key]) ->
    ets:lookup(Tbl, Key).

delete_node(Node) ->
    ets:delete(nodelist, Node).

delete_tracelist_pattern([Node, Cookie], TracePattern) ->
    case lookup([trc_pattern, Node, Cookie]) of
        [] ->
            [];
        [{{Node, Cookie},TracePatterns}] ->
            Updated=lists:delete(TracePattern, TracePatterns),
            ets:insert(tracelist, {{Node, Cookie},Updated})
    end.

truncate_tracelist([Node, Cookie]) ->
    ets:delete(tracelist, {Node, Cookie}).