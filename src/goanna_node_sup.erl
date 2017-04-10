-module(goanna_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/3,
         delete_child/1
]).

%% Supervisor callbacks
-export([init/1]).
-export([id/2, to_node/1]).

-include_lib("goanna.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args),
    {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

-define(CHILD(Id, Mod, Func, Type, Args),
    {Id, {Mod, Func, Args}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [application:get_env(goanna, nodes, [])]).

-spec start_child(node(), atom(), erlang_distribution | file | tcpip_port) -> {ok, supervisor:child()} | {error,{already_started,pid()}}.
start_child(Node, Cookie, Type) ->
	%%?DEBUG("Starting Child ~p ~p ~p", [Node, Cookie, Type]),
    ChildId = id(Node,Cookie),
    case whereis(ChildId) of
        undefined ->
            {ok, NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
            supervisor:start_child(?MODULE, ?CHILD(ChildId, goanna_node, worker, [NodeObj]));
        ChildIdPid ->
            {error,{already_started,ChildIdPid}}
    end.

-spec delete_child(node()) -> ok | {error, no_such_node}.
delete_child(Node) ->
	%%?DEBUG("Deleting Child ~p", [Node]),
    case goanna_db:lookup([nodelist, Node]) of
        [{Node, Cookie,_}] ->
            ID=id(Node,Cookie),
            ok = supervisor:terminate_child(?MODULE, ID),
            ok = supervisor:delete_child(?MODULE, ID);
        [] ->
            {error, no_such_node}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(list()) -> {ok,term()}.
init([SysConfNodes]) when is_list(SysConfNodes) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    lists:foreach(fun([{node,Node},{cookie,Cookie},{type,Type}]) ->
        goanna_api:add_node(Node, Cookie, Type)
    end, SysConfNodes),
    {ok, {SupFlags, []}}.

-spec id(node(), atom()) -> atom().
id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++?NODE_COOKIE_SEP++atom_to_list(Cookie)).

-spec to_node(atom()) -> list().
to_node(ChildId) when is_atom(ChildId) ->
    [Node, Cookie] = string:tokens(atom_to_list(ChildId), ?NODE_COOKIE_SEP),
    [list_to_atom(Node), list_to_atom(Cookie)].
