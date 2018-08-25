-module (goanna_db).
-export ([init/0,
          init_node/1,
          nodes/0,
          store_trace/3,
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
          push/4
]).
-ifdef(TEST).
-export([
    create_node_obj/3
]).
-endif.

-type cookie() :: atom().
-type type() :: tcpip_port | file.
-type node_obj() :: {node(), cookie(), type()}.

-ifdef(ETS_TAKE).
-define(ETS_TAKE(Tbl, Id), ets:take(Tbl, Id)).
-else.
-define(ETS_TAKE(Tbl, Id),
    begin
        case ets:lookup(Tbl, Id) of
            [] ->
                [];
            [V] ->
                true = ets:delete(Tbl, Id),
                [V]
        end
    end
).
-endif.

%% API
-spec init() -> ok.
init() ->
    nodelist =
        ets:new(nodelist, [public, set, named_table]),
    tracelist =
        ets:new(tracelist, [public, set, named_table]),
    relay_tcpip_allocated_ports =
        ets:new(relay_tcpip_allocated_ports, [public, set, named_table]),
    ok.

-spec init_node(list()) -> {ok, node_obj()}.
init_node([Node, Cookie, Type, ChildId]) ->
    io:format("goanna_db init_node ~p\n", [Node]),
    NodeObj = create_node_obj(Node, Cookie, Type),
    true = ets:insert(nodelist, NodeObj),
    undefined = ets:info(ChildId, size),
    io:format("Create ChildId table \n", []),
    ChildId = ets:new(ChildId, [public, ordered_set, named_table, {read_concurrency, true}]),
    {ok, NodeObj}.

create_node_obj(Node, Cookie, Type) when is_atom(Node) andalso
                                         is_atom(Cookie) andalso
                                         (Type == tcpip_port orelse
                                          Type == file) ->
    {Node, Cookie, Type}.

-spec nodes() -> list().
nodes() ->
    all(nodelist).

-spec store_trace(atom() | list(), node(), {trace_ts, any(), any(), any(), any()} |
                                           {trace_ts, any(), any(), any(), any(), any()})
        -> true | {error, term()}.
store_trace(ChildId, Node, {trace_ts,P,L,I,T}) when is_atom(ChildId) ->
    true = ets:insert(ChildId, {T, Node, {trace_ts,P,L,I,T}});
store_trace(ChildId, Node, {trace_ts,P,L,I,E,T}) when is_atom(ChildId) ->
    true = ets:insert(ChildId, {T, Node, {trace_ts,P,L,I,E,T}}).

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
    io:format("goanna_db delete_node ~p\n", [Node]),
    ChildId = goanna_node_sup:id(Node, Cookie),
    io:format("goanna_db delete child ID table ~p\n", [Node]),
    true = ets:delete(ChildId),
    io:format("goanna_db delete nodelist entry ~p\n", [Node]),
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
            ?ETS_TAKE(ChildId, Key)
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
    [E] = ?ETS_TAKE(ChildId, Key),
    pull(ChildId, Amount-1, NextKey, [E|R]).

-spec push(pid() | atom(), term(), atom(), non_neg_integer()) -> ok.
push(PidOrName, ChildId, Mod, Amount) ->
    push(PidOrName, ChildId, Mod, Amount, ets:first(ChildId)).

-spec push(pid() | atom(), term(), atom(), non_neg_integer(), '$end_of_table' | term()) -> ok.
push(_PidOrName, _ChildId, _Mod, _Amount, '$end_of_table') ->
    ok;
push(_PidOrName, _ChildId, _Mod, 0, _) ->
    ok;
push(PidOrName, ChildId, Mod, Amount, Key) when Amount > 0 ->
    NextKey = ets:next(ChildId, Key),
    [E] = ?ETS_TAKE(ChildId, Key),
    ok = Mod:forward(PidOrName, E),
    push(PidOrName, ChildId, Mod, Amount-1, NextKey).

