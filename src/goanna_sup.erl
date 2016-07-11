-module(goanna_sup).

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
    reloader:start_link(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Node, Cookie, Type) ->
	{ok, NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
	supervisor:start_child(?MODULE, ?CHILD(id(Node,Cookie), goanna, worker, [NodeObj])).

delete_child(Node) ->
	[{Node, Cookie,_}] = goanna_db:lookup([nodelist, Node]),
	ID=id(Node,Cookie),
	ok = supervisor:terminate_child(?MODULE, ID),
	supervisor:delete_child(?MODULE, ID).  

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    goanna_db:init(),
    SysConfNodes = application:get_env(goanna, nodes, []),
    Children = lists:map(fun([{node,Node},{cookie,Cookie},{type,Type}]) ->
            {ok, NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
            ?CHILD(id(Node,Cookie), goanna, worker, [NodeObj])
        end, SysConfNodes),
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Children}}.

id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)).