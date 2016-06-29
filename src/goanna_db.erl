-module (goanna_db).
-export ([
          init/0,
          init_node/1,
          store/2,
          lookup/1,
          terminate_node/1,
          delete_tracelist_pattern/2,
          truncate_tracelist/1
]).

%% API

init() ->
    nodelist = ets:new(nodelist, [public, set, named_table]),
    tracelist = ets:new(tracelist, [public, set, named_table]).

init_node([Node, Cookie]) ->
    true = ets:insert(nodelist, {Node,Cookie}),
    Name = goanna_sup:id(Node, Cookie),
    ets:new(Name, [public, ordered_set, named_table]).

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
    ets:insert(Name, {now(),Trace}).

lookup([trc_pattern, Node, Cookie]) ->
    ets:lookup(tracelist, {Node, Cookie}).

terminate_node([Node, Cookie]) ->
    ets:delete(nodelist, {Node,Cookie}).

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