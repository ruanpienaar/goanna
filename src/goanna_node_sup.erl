-module(goanna_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/3,
         delete_child/1,
         id/2,
         to_node/1
]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("goanna.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args),
    {Id, {Mod, start_link, Args}, permanent, 1000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(node(), atom(), erlang_distribution | file | tcpip_port)
        -> {ok, supervisor:child()} | {error,{already_started,pid()}}.
start_child(Node, Cookie, Type) ->
    ChildId = id(Node,Cookie),
    supervisor:start_child(?MODULE,
        ?CHILD(ChildId, goanna_node, worker, [Node, Cookie, Type, ChildId])
    ).

-spec delete_child(node()) -> ok | {error, no_such_node}.
delete_child(Node) ->
    [{Node, Cookie,_}] = goanna_db:lookup([nodelist, Node]),
    ChildId = id(Node,Cookie),
    ok = supervisor:terminate_child(?MODULE, ChildId),
    ok = supervisor:delete_child(?MODULE, ChildId).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(list()) -> {ok,term()}.
init([]) ->
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, []}}.

-spec id(node(), atom()) -> atom().
id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++?NODE_COOKIE_SEP++atom_to_list(Cookie)).

-spec to_node(atom()) -> list().
to_node(ChildId) when is_atom(ChildId) ->
    [Node, Cookie] = string:tokens(atom_to_list(ChildId), ?NODE_COOKIE_SEP),
    [list_to_atom(Node), list_to_atom(Cookie)].