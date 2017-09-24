    -module (goanna_db).
-export ([init/0,
          init_node/1,
          nodes/0,
          store/2,
          lookup/1,
          all/1,
          delete_node/2,
          delete_child_id_tracelist/2,
          delete_child_id_trace_pattern/3,
          truncate_tracelist/1,
          truncate_traces/1,
          pull/1,
          pull/2,
          push/3
]).

%% API
-spec init() -> relay_tcpip_allocated_ports.
init() ->
    nodelist =
        ets:new(nodelist, [public, set, named_table]),
    tracelist =
        ets:new(tracelist, [public, set, named_table]),
    relay_tcpip_allocated_ports =
        ets:new(relay_tcpip_allocated_ports, [public, set, named_table]).

-spec init_node(list()) -> atom().
init_node([Node, Cookie, Type, ChildId]) ->
    NodeObj = {Node, Cookie, Type},
    true = ets:insert(nodelist, NodeObj),
    undefined = ets:info(ChildId, size),
    ChildId = ets:new(ChildId, [public, ordered_set, named_table]),
    {ok, NodeObj}.

-spec nodes() -> list().
nodes() ->
    ets:tab2list(nodelist).

-spec store(atom() | list(), term()) -> true | {error, term()}.
store(ChildId, Trace) when is_atom(ChildId) ->
    true = ets:insert(ChildId, Trace);
store([trace, ChildId], {trace_ts,P,L,I,T}) ->
    true = ets:insert(ChildId, {T, {trace_ts,P,L,I,T}});
store([trace, ChildId], {trace_ts,P,L,I,E,T}) ->
    true = ets:insert(ChildId, {T, {trace_ts,P,L,I,E,T}});
store([trace, Node, Cookie], Trace) ->
    store([trace, goanna_node_sup:id(Node, Cookie)], Trace);
store([tracelist, ChildId, TrcPattern], Opts) ->
    true = ets:insert(tracelist, {{ChildId, TrcPattern}, Opts}).

-spec lookup(list()) -> term().
lookup([nodelist, Node]) ->
    ets:lookup(nodelist, Node);
lookup([tracelist, ChildId]) ->
    ets:match_object(tracelist, {{ChildId, '_'}, '_'});
lookup([tracelist, ChildId, TrcPattern]) ->
    ets:lookup(tracelist, {ChildId, TrcPattern});
lookup([tracelist, Node, Cookie, TrcPattern]) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ets:lookup(tracelist, {ChildId, TrcPattern});
lookup([trace, Tbl, Key]) ->
    ets:lookup(Tbl, Key).

all(tracelist) ->
    ets:tab2list(tracelist);
all(nodelist) ->
    ets:tab2list(nodelist).

-spec delete_node(atom(), atom()) -> true | {error, term()}.
delete_node(Node, Cookie) ->
    true = ets:delete(goanna_node_sup:id(Node, Cookie)),
    true = ets:delete(nodelist, Node).

-spec delete_child_id_tracelist(node(), atom()) -> ok | {error, term()}.
delete_child_id_tracelist(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    true = ets:match_delete(tracelist, {{ChildId, '_'}, '_'}).

-spec delete_child_id_trace_pattern(node(), atom(), term()) -> ok | {error, term()}.
delete_child_id_trace_pattern(Node, Cookie, TrcPattern) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    true = ets:match_delete(tracelist, {{ChildId, TrcPattern}, '_'}).

-spec truncate_tracelist(list()) -> true | {error, term()}.
truncate_tracelist([]) ->
    ets:delete_all_objects(tracelist).

-spec truncate_traces(term()) -> ok | {error, term()}.
truncate_traces(Tbl) ->
    ok = ets:delete_all_objects(Tbl).

-spec pull(atom()) -> list().
pull(Tbl) ->
    case ets:first(Tbl) of
        '$end_of_table' ->
            [];
        Key ->
            ets:take(Tbl, Key)
    end.

-spec pull(atom(), non_neg_integer()) -> list().
pull(Tbl, Amount) ->
    pull(Tbl, Amount, ets:first(Tbl), []).

-spec pull(atom(), non_neg_integer(), '$end_of_table' | term(), list()) -> ok.
pull(_Tbl, _Amount, '$end_of_table', []) ->
    [];
pull(_Tbl, _Amount, '$end_of_table', R) ->
    lists:reverse(R);
pull(_Tbl, 0, _Key, R) ->
    lists:reverse(R);
pull(Tbl, Amount, Key, R) when Amount > 0 ->
    NextKey = ets:next(Tbl, Key),
    [E] = ets:take(Tbl, Key),
    pull(Tbl, Amount-1, NextKey, [E|R]).

-spec push(term(), atom(), non_neg_integer()) -> ok.
push(Tbl, Mod, Amount) ->
    push(Tbl, Mod, Amount, ets:first(Tbl)).

-spec push(term(), atom(), non_neg_integer(), '$end_of_table' | term()) -> ok.
push(_Tbl, _Mod, _Amount, '$end_of_table') ->
    ok;
push(_Tbl, _Mod, 0, _) ->
    ok;
push(Tbl, Mod, Amount, Key) when Amount > 0 ->
    NextKey = ets:next(Tbl, Key),
    [E] = ets:take(Tbl, Key),
    ok = Mod:forward(Tbl, E),
    push(Tbl, Mod, Amount-1, NextKey).
