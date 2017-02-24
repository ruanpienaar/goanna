-module (goanna_db).
-export ([init/0,
          init_node/1,
          node_table_exists/2,
          nodes/0,
          store/2,
          lookup/1,
          delete_node/1,
          delete_child_id_tracelist/2,
          delete_child_id_trace_pattern/3,
          truncate_tracelist/1,
          truncate_traces/1,
          pull/1,
          pull/2
]).

%% API
-spec init() -> relay_tcpip_allocated_ports.
init() ->
    nodelist = ets:new(nodelist, [public, set, named_table]),
    tracelist = ets:new(tracelist, [public, set, named_table]),
    relay_tcpip_allocated_ports = ets:new(relay_tcpip_allocated_ports, [public, set, named_table]).

-spec init_node(list()) -> atom().
init_node([Node, Cookie, Type]) ->
    NodeObj = {Node, Cookie, Type},
    true = ets:insert(nodelist, NodeObj),
    ChildId = goanna_node_sup:id(Node, Cookie),
    case ets:info(ChildId, size) of
        undefined ->
            ChildId = ets:new(ChildId, [public, ordered_set, named_table]);
        Size when is_integer(Size) -> %% Table already exists, why would it exist?
            ok
    end,
    {ok, NodeObj}.

-spec node_table_exists(node(), atom()) -> boolean().
node_table_exists(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    case ets:info(ChildId, size) of
        undefined            -> false;
        S when is_integer(S) -> true
    end.

-spec nodes() -> list().
nodes() ->
    ets:tab2list(nodelist).

-spec store(atom() | list(), term()) -> ok | {error, term()}.
store(ChildId, Trace) when is_atom(ChildId) ->
    ets:insert(ChildId, Trace);

store([trace, ChildId], {trace_ts,P,L,I,T}) ->
    ets:insert(ChildId, {T, {trace_ts,P,L,I,T}});
store([trace, ChildId], {trace_ts,P,L,I,E,T}) ->
    ets:insert(ChildId, {T, {trace_ts,P,L,I,E,T}});
store([trace, Node, Cookie], Trace) ->
    store([trace, goanna_node_sup:id(Node, Cookie)], Trace);
store([tracelist, ChildId, TrcPattern], Opts) ->
    ets:insert(tracelist, {{ChildId, TrcPattern}, Opts}).

-spec lookup(list()) -> term().
lookup([nodelist, Node]) ->
    ets:lookup(nodelist, Node);
lookup([trc_pattern, ChildId, TrcPattern]) ->
    ets:lookup(tracelist, {ChildId, TrcPattern});
lookup([trc_pattern, Node, Cookie, TrcPattern]) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:lookup(tracelist, {ChildId, TrcPattern});
lookup([trace, Tbl, Key]) ->
    ets:lookup(Tbl, Key).

-spec delete_node(atom()) -> ok | {error, term()}.
delete_node(Node) ->
    ets:delete(nodelist, Node).

-spec delete_child_id_tracelist(node(), atom()) -> ok | {error, term()}.
delete_child_id_tracelist(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:match_delete(tracelist, {{ChildId, '_'}, '_'}).

-spec delete_child_id_trace_pattern(node(), atom(), term()) -> ok | {error, term()}.
delete_child_id_trace_pattern(Node, Cookie, TrcPattern) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:match_delete(tracelist, {{ChildId, TrcPattern}, '_'}).

-spec truncate_tracelist(list()) -> [] | {error, term()}.
truncate_tracelist([]) ->
    ets:delete_all_objects(tracelist).

-spec truncate_traces(term()) -> ok | {error, term()}.
truncate_traces(Tbl) ->
    ets:delete_all_objects(Tbl).

-spec first(atom()) -> '$end_of_table' | term().
first(Tbl) ->
	ets:first(Tbl).

-spec next(term(), term()) -> '$end_of_table' | term().
next(Tbl, Continuation) ->
	ets:next(Tbl, Continuation).

end_of_table() ->
	'$end_of_table'.

lookup_entry(Tbl, Key) ->
	ets:lookup(Tbl, Key).

delete(Tbl, Key) ->
	ets:delete(Tbl, Key).

-spec pull(atom()) -> list().
pull(Tbl) ->
	pull(Tbl, 1).

-spec pull(atom(), non_neg_integer()) -> list().
pull(Tbl, BatchSize) ->
	End=end_of_table(),
	case first(Tbl) of
		End ->
			[];
		FirstKey ->
			pull(Tbl, BatchSize, FirstKey, [])
	end.

pull(_Tbl, _BatchSize, '$end_of_table', R) ->
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
