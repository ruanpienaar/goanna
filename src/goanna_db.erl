-module (goanna_db).
-export ([
          init/0,
          init_node/1,
          node_table_exists/2,
          nodes/0,
          store/2,
          lookup/1,
          delete_node/1,
          delete_child_id_tracelist/2,
          truncate_tracelist/1,
          truncate_traces/1,
          pull/1,
          pull/2
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
    case ets:info(ChildId, size) of
        undefined ->
            ChildId = ets:new(ChildId, [public, ordered_set, named_table]);
        Size when is_integer(Size) ->
            ok
    end,
    {ok, NodeObj}.

node_table_exists(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    case ets:info(ChildId, size) of
        undefined            -> false;
        S when is_integer(S) -> true
    end.

nodes() ->
    ets:tab2list(nodelist).

store(ChildId, Trace) when is_atom(ChildId) ->
	ets:insert(ChildId, Trace);
store([trace, ChildId], Trace) ->
    ets:insert(ChildId, {?GOANNA_NOW(),Trace});
store([trace, Node, Cookie], Trace) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:insert(ChildId, {?GOANNA_NOW(),Trace});
store([tracelist, ChildId, TrcPattern], Opts) ->
    ets:insert(tracelist, {{ChildId, TrcPattern}, ?GOANNA_NOW(), Opts}).

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

delete_child_id_tracelist(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:match_delete(tracelist, {{ChildId, '_'}, '_'}).

truncate_tracelist([]) ->
    ets:delete_all_objects(tracelist).

truncate_traces(Tbl) ->
    ets:delete_all_objects(Tbl).

first(Tbl) ->
	ets:first(Tbl).

next(Tbl, Continuation) ->
	ets:next(Tbl, Continuation).

end_of_table() ->
	'$end_of_table'.

lookup_entry(Tbl, Key) ->
	ets:lookup(Tbl, Key).

delete(Tbl, Key) ->
	ets:delete(Tbl, Key).

pull(Tbl) ->
	pull(Tbl, 1).

pull(Tbl, BatchSize) ->
	End=end_of_table(),
	case first(Tbl) of
		End ->
			[];
		FirstKey ->
			pull(Tbl, BatchSize, FirstKey, [])
	end.

pull(Tbl, BatchSize, '$end_of_table', R) ->
	lists:reverse(R);
pull(_Tbl, BatchSize, _Key, R) when BatchSize =< 0 ->
	lists:reverse(R);
pull(Tbl, BatchSize, Key, R) when is_integer(BatchSize) ->
	case lookup_entry(Tbl, Key) of
		[] ->
			lists:reverse(R);
		[Entry] ->
			true = delete(Tbl, Key),
			pull(Tbl, BatchSize-1, next(Tbl,Key), [Entry|R])
	end.
