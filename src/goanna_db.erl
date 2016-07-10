-module (goanna_db).
-export ([
          init/0,
          init_node/1,
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
    Name = goanna_sup:id(Node, Cookie),
    Name = ets:new(Name, [public, ordered_set, named_table]),
    {ok, NodeObj}.

store([trace_pattern, Node, Cookie], TracePattern) ->
    case ets:lookup(tracelist, {Node, Cookie}) of
        [{{Node, Cookie}, TracePatterns}] ->
            case lists:member(TracePattern, TracePatterns) of
                true ->
                    {error, already_traced};
                false ->
                    ets:insert(tracelist, {{Node, Cookie},[TracePattern|TracePatterns]})
            end;
        [] ->
            ets:insert(tracelist, {{Node, Cookie},[TracePattern]})
    end;
store([trace, Node, Cookie], Trace) ->
    Name = goanna_sup:id(Node, Cookie),
    ets:insert(Name, {erlang:timestamp(),Trace}).

lookup([nodelist, Node]) ->
    ets:lookup(nodelist, Node);
lookup([trc_pattern, Node, Cookie]) ->
    ets:lookup(tracelist, {Node, Cookie});
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