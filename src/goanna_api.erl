-module (goanna_api).

-compile({no_auto_import,[nodes/0]}).

%% Control Api
-export([
    start/0, stop/0,
    add_node/2,
    add_node/3,
    add_node_callbacks/2,
    add_node_callbacks/3,
    remove_node/1,
    remove_goanna_node/1,
    remove_goanna_callbacks/1,
    nodes/0,
    update_default_trace_options/1,
    set_data_retrival_method/1
]).

%% Trace Api
-export([
    trace/1, trace/2, trace/3,
    trace_ms/1,
    trace_modules/1,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3,
    clear_all_traces/0,
    % recv_trace/2,
    list_active_traces/0
]).

%% Traces
-export([
    pull_all_traces/0,
    pull_traces/1
]).

-include_lib("goanna.hrl").

%%------------------------------------------------------------------------
%% Goanna start-up helper only
start() ->
    {ok, _} = application:ensure_all_started(goanna).

stop() ->
    ok = application:stop(goanna).
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% API
-spec nodes() -> list(proplists:proplist()).
nodes() ->
    goanna_db:all(nodelist).

% TODO: implement file TYPE
-spec add_node(node(), atom()) ->
        hawk_sup:start_child_return() | {error, badarg}.
add_node(Node, Cookie) when is_atom(Node), is_atom(Cookie) ->
    add_node(Node, Cookie, tcpip_port).

add_node(Node, Cookie, Type) when Type =:= tcpip_port orelse
                                  Type =:= file ->
    case hawk:node_exists(Node) of
        {error, no_such_node} ->
            {ok, _Pid} =
                hawk:add_node(Node, Cookie,
                    goanna_connect_callbacks(Node, Cookie, Type),
                    goanna_disconnect_callbacks(Node, Cookie)
                );
        % {error, connecting} ->
            % TODO: check callbacks
            % if present, just carry on, if NOT add callbacks
        %     ok;
        {ok, Pid, _CallbackNames} ->
            % TODO: check callbacks
            % if present, just carry on, if NOT add callbacks
            {ok, {Pid, updated}} = hawk:remove_connect_callback(Node, goanna_connect),
            {ok, {Pid, updated}} = hawk:remove_disconnect_callback(Node, goanna_disconnect),
            goanna_log:log("removed old callbacks...~n", []),
            ok = add_node_callbacks(Node, Cookie, Type),
            {ok, Pid}
    end;
add_node(_, _, _) ->
    {error, badarg}.

add_node_callbacks(Node, Cookie) ->
    add_node_callbacks(Node, Cookie, tcpip_port).

add_node_callbacks(Node, Cookie, Type) ->
    % Maybe check if they are not already there...
    % {ok, _Pid, _Callbacks} = hawk:node_exists(Node),
    [{CN,CF}] = goanna_connect_callbacks(Node, Cookie, Type),
    [{DN,DF}] = goanna_disconnect_callbacks(Node, Cookie),
    case hawk:add_connect_callback(Node, {CN,CF}) of
        {ok, {_,duplicate}} ->
            ok;
        {ok, {_, updated}} ->
            ok
    end,
    case hawk:add_disconnect_callback(Node, {DN,DF}) of
        {ok, {_,duplicate}} ->
            ok;
        {ok, {_, updated}} ->
            ok
    end.

goanna_connect_callbacks(Node, Cookie, Type) ->
    [{goanna_connect, fun() ->
        goanna_log:log("Starting Goanna Node\n", []),
        {ok,_}=goanna_node_sup:start_child(Node, Cookie, Type) end}].

goanna_disconnect_callbacks(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node,Cookie),
    [{goanna_disconnect, fun() ->
        goanna_log:log("Stopping Goanna Node\n", []),
        ok=goanna_node_sup:delete_child(ChildId) end}].

% @doc removes from goanna and hawk, by using the disconnect
% callbacks
% @doc
-spec remove_node(node()) -> ok | {error, no_such_node}.
remove_node(Node) when is_atom(Node) ->
    hawk:remove_node(Node);
remove_node(_) ->
    {error, badarg}.

%%TODO: test
% @doc only removes the goanna node
% @doc
-spec remove_goanna_node(node()) -> ok | {error, no_such_node}.
remove_goanna_node(Node) ->
    case remove_goanna_callbacks(Node) of
        true ->
            case goanna_db:lookup([nodelist, Node]) of
                [{Node, Cookie,_}] ->
                    ChildId = goanna_node_sup:id(Node,Cookie),
                    goanna_node_sup:delete_child(ChildId);
                [] ->
                    ok
            end;
        false ->
            ok
    end.

-spec remove_goanna_callbacks(node()) -> boolean().
remove_goanna_callbacks(Node) ->
    case
        {hawk:remove_connect_callback(Node, goanna_connect),
         hawk:remove_disconnect_callback(Node, goanna_disconnect)
        }
    of
        {{ok, {Pid, updated}}, {ok, {Pid, updated}}
        } ->
            true;
        _ ->
            false
    end.

-spec update_default_trace_options(list(tuple())) -> ok.
update_default_trace_options(Opts) when is_list(Opts) ->
    DefaultOpts = application:get_env(goanna, default_trace_options, []),
    F = fun
        ({Field, false}, DefOpts) ->
            lists:keydelete(Field, 1, DefOpts);
        ({time, Time}, DefOpts) when is_integer(Time) ->
            lists:keystore(time, 1, DefOpts, {time, Time});
        ({messages, Msgs}, DefOpts) when is_integer(Msgs) ->
            lists:keystore(messages, 1, DefOpts, {messages, Msgs})
    end,
    NewDefaultOpts = lists:foldl(F, DefaultOpts, Opts),
    ok = application:set_env(goanna, default_trace_options, NewDefaultOpts),
    cluster_foreach_call({update_default_trace_options});
update_default_trace_options(_) ->
    {error, badarg}.

%% TODO: create a separate call, for all the other sys.config app-env values...
-spec set_data_retrival_method({push, non_neg_integer(), atom()} | pull) -> ok.
set_data_retrival_method(DRM=pull) ->
    ok = application:set_env(goanna, data_retrival_method, DRM),
    cluster_foreach_call({update_data_retrival_method});
set_data_retrival_method(DRM={push, _Interval, _Mod, _Amount}) ->
    ok = application:set_env(goanna, data_retrival_method, DRM),
    cluster_foreach_call({update_data_retrival_method});
set_data_retrival_method(_) ->
    {error, badarg}.

-spec trace(atom()) -> ok | {error, badarg}.
trace(Module) when is_atom(Module) ->
    try
        %cluster_foreach_call({trace, [#trc_pattern{m=Module}]})
        cluster_foreach_call({trace, [{Module}]})
    catch
        C:R ->
            error_logger:error_report([{module,?MODULE},C,R])
    end;
trace(_) ->
    {error, badarg}.

-spec trace(atom(), atom()) -> ok | {error, badarg}.
trace(Module, Function) when is_atom(Module), is_atom(Function) ->
    % cluster_foreach_call({trace, [#trc_pattern{m=Module,f=Function}]});
    cluster_foreach_call({trace, [{Module, Function}]});
trace(_, _) ->
    {error, badarg}.

-spec trace(atom(), atom(), integer()) -> ok | {error, badarg}.
trace(Module, Function, Arity) when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    % cluster_foreach_call({trace, [#trc_pattern{m=Module,f=Function,a=Arity}]});
    cluster_foreach_call({trace, [{Module, Function, Arity}]});
trace(_, _, _) ->
    {error, badarg}.

-spec trace_ms(string()) -> ok | {error, badarg}.
trace_ms(MsStr) when is_list(MsStr) ->
    {{M,F,A},MatchSpec,[_Flag]} = redbug_msc:transform(MsStr),
    % cluster_foreach_call({trace, [#trc_pattern{m=M,f=F,a=A,ms=MatchSpec}]}).
    cluster_foreach_call({trace, [{M,F,A,MatchSpec}]}).

-spec trace_modules(list( atom() )) -> ok.
trace_modules(Modules) ->
    trace_modules(Modules, [{time, false}, {messages, false}]).

-spec trace_modules(list(), list()) -> ok.
trace_modules(Modules, Opts) ->
    cluster_foreach_call_infinity({trace, Opts, [ {M} || M <- Modules,
        %% TODO: build some safetynet...
        ( M /= erlang orelse
          M /= lists orelse
          M /= io )
    ]}).

-spec stop_trace() -> ok.
stop_trace() ->
    cluster_foreach_call({stop_all_trace_patterns}).

%% TODO: add stop_trace_ms
-spec stop_trace(atom()) -> ok.
stop_trace(Module) ->
    % cluster_foreach_call({stop_trace, #trc_pattern{m=Module}}).
    cluster_foreach_call({stop_trace, {Module}}).

-spec stop_trace(atom(), atom()) -> ok.
stop_trace(Module, Function) ->
    %cluster_foreach_call({stop_trace, #trc_pattern{m=Module,f=Function}}).
    cluster_foreach_call({stop_trace, {Module, Function}}).

-spec stop_trace(atom(), atom(), integer()) -> ok.
stop_trace(Module, Function, Arity) ->
    % cluster_foreach_call({stop_trace, #trc_pattern{m=Module,f=Function,a=Arity}}).
    cluster_foreach_call({stop_trace, {Module, Function, Arity}}).

-spec clear_all_traces() -> ok.
clear_all_traces() ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            ChildId = goanna_node_sup:id(Node, Cookie),
            goanna_db:truncate_traces(ChildId)
        end, ets:tab2list(nodelist)
    ).

-spec list_active_traces() -> list().
list_active_traces() ->
    goanna_db:all(tracelist).

-spec pull_all_traces() -> list().
pull_all_traces() ->
    pull_traces(50).

-spec pull_traces(non_neg_integer()) -> list().
pull_traces(Size) ->
    Nodes = ?MODULE:nodes(),
    F = fun({Node,Cookie,_}, Acc) ->
        case goanna_db:pull(goanna_node_sup:id(Node,Cookie), Size) of
          [] ->
            Acc;
          Traces ->
            %% TODO: add node, so that the FE can know
            %% Which traces was for what...
            [Traces|Acc]
        end
    end,
    lists:flatten(lists:foldl(F, [], Nodes)).

%% -------

cluster_foreach_call(Cmd) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            ChildId = goanna_node_sup:id(Node, Cookie),
            call(ChildId, Cmd)
        end, ets:tab2list(nodelist)
    ).

cluster_foreach_call_infinity(Cmd) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            ChildId = goanna_node_sup:id(Node, Cookie),
            call_node_infinity(ChildId, Cmd)
        end, ets:tab2list(nodelist)
    ).

call(ChildId, Cmd) ->
    whereis(ChildId) ! {Cmd, self()},
    receive
        ok ->
            ok
    after
        10000 ->
            timeout
    end.

call_node_infinity(ChildId, Cmd) ->
    whereis(ChildId) ! {Cmd, self()},
    receive
        ok ->
            ok
    end.