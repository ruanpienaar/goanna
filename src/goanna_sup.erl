-module(goanna_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    goanna_db:init(),
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [
    				 %% ?CHILD(rrr, reloader, worker, []),
                     ?CHILD(gnm, goanna_node_manager, worker, []),
                     ?CHILD(gns, goanna_node_sup, supervisor, []),
                     ?CHILD(gtc, goanna_trace_collector, worker, [])
                    ]}}.