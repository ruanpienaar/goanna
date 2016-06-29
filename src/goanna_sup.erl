-module(goanna_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    %%
    % try
    %     dbg:cn(node())
    % catch
    %     _C:_E ->
    %         ok
    % end,
    R.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    goanna_db:init(),
    SysConfNodes = application:get_env(goanna, nodes, []),
    Children = lists:map(fun({Node, Cookie}) ->
            goanna_db:init_node([Node, Cookie]),
            ?CHILD(id(Node,Cookie), goanna, worker, [Node, Cookie])
        end, SysConfNodes),

    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [?CHILD(goanna_tracer, goanna_tracer, worker, [])
                     % ?CHILD(goanna_monitor, goanna_monitor, worker, [])
                    ] ++ Children}}.

id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)).