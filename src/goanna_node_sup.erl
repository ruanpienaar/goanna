-module(goanna_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/3, start_child_sync/3,
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

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [application:get_env(goanna, nodes, [])]).

start_child(Node, Cookie, Type) ->
    ChildId = id(Node,Cookie),
    case whereis(ChildId) of
        undefined ->
            {ok, NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
            supervisor:start_child(?MODULE, ?CHILD(ChildId, goanna_node, worker, [NodeObj]));
        ChildIdPid ->
            {error,{already_started,ChildIdPid}}
    end.

start_child_sync(Node, Cookie, Type) ->
    ChildId = id(Node,Cookie),
    case whereis(ChildId) of
        undefined ->
            {ok, NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
            supervisor:start_child(?MODULE, ?CHILD(ChildId, goanna_node, worker, [NodeObj, sync]));
        ChildIdPid ->
            {error,{already_started,ChildIdPid}}
    end.

delete_child(Node) ->
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
init([SysConfNodes]) when is_list(SysConfNodes) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    lists:map(fun([{node,Node},{cookie,Cookie},{type,Type}]) ->
        CCB = fun() -> goanna_node_sup:start_child(Node, Cookie, Type) end,
        DCB = fun() -> goanna_node_sup:delete_child(Node) end,
        {ok, Pid} = hawk:add_node(Node, Cookie, CCB, DCB)
    end, SysConfNodes),
    {ok, {SupFlags, []}}.

id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++?NODE_COOKIE_SEP++atom_to_list(Cookie)).

to_node(ChildId) when is_atom(ChildId) ->
    [Node, Cookie] = string:tokens(atom_to_list(ChildId), ?NODE_COOKIE_SEP),
    [list_to_atom(Node), list_to_atom(Cookie)].
