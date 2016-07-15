-module(goanna_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/3,
         delete_child/1
]).

%% Supervisor callbacks
-export([init/1]).
-export([id/2]).

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
    supervisor:start_child(?MODULE, ?CHILD(id(Node,Cookie), goanna_node, worker, [{Node, Cookie, Type}])).

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

init([SysConfNodes]) ->
    Children
        = lists:map(fun([{node,Node},{cookie,Cookie},{type,Type}]) ->
            %% I'd like to keep the ets table's entries, even after restarts.
            {ok, NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
            ?CHILD(id(Node,Cookie), goanna_node, worker, [NodeObj])
        end, SysConfNodes),
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Children}}.

id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)).
