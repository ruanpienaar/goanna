-module (goanna_db).
-export ([init/0,
          init_node/1,
          nodes/0,
          store_trace/2,
          store_trace_pattern/3,
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

-type cookie() :: atom().
-type type() :: tcpip_port | file.
-type node_obj() :: {node(), cookie(), type()}.

%% API
-spec init() -> relay_tcpip_allocated_ports.
init() ->
    nodelist =
        ets:new(nodelist, [public, set, named_table]),
    tracelist =
        ets:new(tracelist, [public, set, named_table]),
    relay_tcpip_allocated_ports =
        ets:new(relay_tcpip_allocated_ports, [public, set, named_table]).

-spec init_node(list()) -> {ok, node_obj()}.
init_node([Node, Cookie, Type, ChildId]) ->
    NodeObj = {Node, Cookie, Type},
    true = ets:insert(nodelist, NodeObj),
    undefined = ets:info(ChildId, size),
    ChildId = ets:new(ChildId,[public, ordered_set, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, NodeObj}.

-spec nodes() -> list().
nodes() ->
    all(nodelist).

-spec store_trace(atom() | list(), {trace_ts, any(), any(), any(), any()} |
                                   {trace_ts, any(), any(), any(), any(), any()})
        -> true | {error, term()}.
store_trace(ChildId, {trace_ts,P,L,I,T}) when is_atom(ChildId) ->
    true = ets:insert(ChildId, {T, {trace_ts,P,L,I,T}});
store_trace(ChildId, {trace_ts,P,L,I,E,T}) when is_atom(ChildId) ->
    true = ets:insert(ChildId, {T, {trace_ts,P,L,I,E,T}}).

-spec store_trace_pattern(atom() | list(), list(), list()) -> true | {error, term()}.
store_trace_pattern(ChildId, TrcPattern, Opts) ->
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
    ets:lookup(tracelist, {ChildId, TrcPattern}).

all(tracelist) ->
    ets:tab2list(tracelist);
all(nodelist) ->
    ets:tab2list(nodelist);
all(_Tbl) ->
    [].

-spec delete_node(atom(), atom()) -> true | {error, term()}.
delete_node(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    true = ets:delete(ChildId),
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
truncate_traces(ChildId) ->
    true = ets:delete_all_objects(ChildId).

-spec pull(atom()) -> list().
pull(ChildId) ->
    case ets:first(ChildId) of
        '$end_of_table' ->
            [];
        Key ->
            ets:take(ChildId, Key)
    end.

-spec pull(atom(), non_neg_integer()) -> list().
pull(ChildId, Amount) ->
    pull(ChildId, Amount, ets:first(ChildId), []).

-spec pull(atom(), non_neg_integer(), '$end_of_table' | term(), list()) -> ok.
pull(_ChildId, _Amount, '$end_of_table', []) ->
    [];
pull(_ChildId, _Amount, '$end_of_table', R) ->
    lists:reverse(R);
pull(_ChildId, 0, _Key, R) ->
    lists:reverse(R);
pull(ChildId, Amount, Key, R) when Amount > 0 ->
    NextKey = ets:next(ChildId, Key),
    [E] = ets:take(ChildId, Key),
    pull(ChildId, Amount-1, NextKey, [E|R]).

-spec push(term(), atom(), non_neg_integer()) -> ok.
push(ChildId, Mod, Amount) ->
    push(ChildId, Mod, Amount, ets:first(ChildId)).

-spec push(term(), atom(), non_neg_integer(), '$end_of_table' | term()) -> ok.
push(_ChildId, _Mod, _Amount, '$end_of_table') ->
    ok;
push(_ChildId, _Mod, 0, _) ->
    ok;
push(ChildId, Mod, Amount, Key) when Amount > 0 ->
    NextKey = ets:next(ChildId, Key),
    [E] = ets:take(ChildId, Key),
    ok = Mod:forward(ChildId, E),
    push(ChildId, Mod, Amount-1, NextKey).
