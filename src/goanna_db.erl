-module (goanna_db).
-export ([
          init/0,
          init_node/1,
          node_table_exists/2,
          store/2,
          lookup/1,
          delete_node/1,
          delete_tracelist_pattern/2,
          truncate_tracelist/1,
          truncate_traces/1
]).

%% API

%% TODO: allow older erlang versions, to use the legacy erlang now()
%% possibly update the macro below with a -ifdef, checking versions,
%% or do a complete overhaul to rebar3
-define(GOANNA_NOW(), erlang:timestamp()).

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
    ets:insert(ChildId, {?GOANNA_NOW(),Trace});
store([trace, Node, Cookie], Trace) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:insert(ChildId, {?GOANNA_NOW(),Trace});
store([tracelist, ChildId], TrcPattern) ->
    ets:insert(tracelist, {{ChildId, TrcPattern}, ?GOANNA_NOW()}).

lookup([nodelist, Node]) ->
    ets:lookup(nodelist, Node);
lookup([trc_pattern, ChildId, TrcPattern]) ->
    ets:lookup(tracelist, {ChildId, TrcPattern});
lookup([trc_pattern, Node, Cookie, TrcPattern]) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:lookup(tracelist, {ChildId, TrcPattern});
lookup([trace, Tbl, Key]) ->
    ets:lookup(Tbl, Key).

delete_node(Node) ->
    ets:delete(nodelist, Node).

delete_tracelist_pattern([Node, Cookie], TrcPattern) ->
    case lookup([trc_pattern, Node, Cookie]) of
        [] ->
            [];
        [{{Node, Cookie},TracePatterns}] ->
            Updated=lists:delete(TrcPattern, TracePatterns),
            ets:insert(tracelist, {{Node, Cookie},Updated})
    end.

truncate_tracelist([]) ->
    ets:delete_all_objects(tracelist).

truncate_traces(ChildId) ->
    ets:delete_all_objects(ChildId).
