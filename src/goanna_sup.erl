-module(goanna_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([id/2]).
-export([trace/1]).
-export([stop_trace/0]).

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
    nodelist = ets:new(nodelist, [public, set, named_table]),
    SysConfNodes = application:get_env(goanna, nodes, []),
    Children = lists:map(fun({Node, Cookie}) ->
            ?CHILD(id(Node, Cookie), goanna, worker, [Node, Cookie])
        end, SysConfNodes),

    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [?CHILD(goanna_tracer, goanna_tracer, worker, []),
                     ?CHILD(goanna_monitor, goanna_monitor, worker, [])
                    ] ++ Children}}.

id(Node, Cookie) ->
    list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)).

trace(Module) ->
    lists:foreach(
        fun({Exclude,_Pid,_,[Exclude]}) when Exclude =:= goanna_tracer;
                                             Exclude =:= goanna_monitor ->
            ok;
           ({_Id,Pid,worker,_Modules}) ->
            Pid ! {trace, Module}
        end,
        supervisor:which_children(?MODULE)
    ).

stop_trace() ->
    lists:foreach(
        fun({Exclude,_Pid,_,[Exclude]}) when Exclude =:= goanna_tracer;
                                             Exclude =:= goanna_monitor ->
            ok;
           ({_Id,Pid,worker,_}) ->
            Pid ! stop_trace
        end,
        supervisor:which_children(?MODULE)
    ).