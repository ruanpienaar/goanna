-module (goanna_api).

%% Control Api
-export([
    start/0, stop/0,
    add_node/3,
    remove_node/1,
    nodes/0,
    update_default_trace_options/1,
    set_data_retrival_method/1
]).

%% Trace Api
-export([
    trace/1, trace/2, trace/3, trace/4,
    trc/1,
    trace_modules/1,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3,
    clear_all_traces/0,
    recv_trace/2,
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
-spec start() -> list().
start() -> [ ok = application:ensure_started(APP) || APP <- apps() ].
-spec stop() -> list().
stop() -> [ ok = application:ensure_started(APP) || APP <- lists:reverse(apps()) ].
-spec apps() -> list().
apps() ->
    [asn1, crypto, public_key, ssl, compiler, inets, syntax_tools, sasl,
     hawk, goanna].
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% API
-spec nodes() -> list(proplists:proplist()).
nodes() ->
    HawkNodes = hawk:nodes(),
    lists:filter(
        fun({Node,_Cookie,_Type}) ->
            lists:member(Node, HawkNodes)
        end, goanna_db:nodes()
    ).

-spec add_node(node(), atom(), erlang_distribution | file | tcpip_port) ->
        {ok, pid()} | {error,{already_started,pid()}}.
add_node(Node, Cookie, Type) when is_atom(Node),
                                  is_atom(Cookie),
                                  (Type == erlang_distribution orelse
                                   Type == file orelse
                                   Type == tcpip_port) ->
    ConnectedCallBack = [{goanna_connect, fun() -> {ok,_}=goanna_node_sup:start_child(Node, Cookie, Type) end}],
    DisconnCallBack = [{goanna_disconnect, fun() -> ok=goanna_node_sup:delete_child(Node) end}],
    case hawk:node_exists(Node) of
        {ok, Pid, Callbacks} ->
            case lists:member(goanna_connect, Callbacks) andalso lists:member(goanna_disconnect, Callbacks) of
                true ->
                    ok;
                false ->
                    [{CName,CF}] = ConnectedCallBack,
                    [{DName,DCF}] = DisconnCallBack,
                    CF(),
                    {ok,updated} = hawk:add_connect_callback(Node, {CName,CF}),
                    {ok,updated} = hawk:add_disconnect_callback(Node, {DName,DCF})
            end,
            {error,{already_started,Pid}};
        false ->
            hawk:add_node(Node, Cookie, ConnectedCallBack, DisconnCallBack)
    end;
add_node(_, _, _) ->
    {error, badarg}.

-spec remove_node(node()) -> ok | {error, no_such_node}.
remove_node(Node) when is_atom(Node) ->
    hawk:remove_node(Node);
remove_node(_) ->
    {error, badarg}.

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
    application:set_env(goanna, default_trace_options, NewDefaultOpts),
    cluster_foreach_call({update_state});
update_default_trace_options(_) ->
    {error, badarg}.

%% TODO: create a separate call, for all the other sys.config app-env values...
-spec set_data_retrival_method({push, non_neg_integer(), atom()} | pull) -> ok.
set_data_retrival_method(DRM=pull) ->
    ok = application:set_env(goanna, data_retrival_method, DRM),
    cluster_foreach_call({update_state});
set_data_retrival_method(DRM={push, _Interval, _Mod}) ->
    ok = application:set_env(goanna, data_retrival_method, DRM),
    cluster_foreach_call({update_state});
set_data_retrival_method(_) ->
    {error, badarg}.

-spec trace(atom()) -> ok | {error, badarg}.
trace(Module) when is_atom(Module) ->
    try
        cluster_foreach_call({trace, [], [#trc_pattern{m=Module}]})
    catch
        C:E ->
            io:format("trace failed ~p ~p", [C,E])
    end;
trace(_) ->
    {error, badarg}.

-spec trace(atom(), atom()) -> ok | {error, badarg}.
trace(Module, Function) when is_atom(Module), is_atom(Function) ->
    cluster_foreach_call({trace, [], [#trc_pattern{m=Module,f=Function}]});
trace(_, _) ->
    {error, badarg}.

-spec trace(atom(), atom(), integer()) -> ok | {error, badarg}.
trace(Module, Function, Arity) when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    cluster_foreach_call({trace, [], [#trc_pattern{m=Module,f=Function,a=Arity}]});
trace(_, _, _) ->
    {error, badarg}.

-spec trace(atom(), atom(), integer(), list()) -> ok | {error, badarg}.
trace(Module, Function, Arity, Opts) when is_atom(Module), is_atom(Function), is_integer(Arity), is_list(Opts) ->
    cluster_foreach_call({trace, Opts, [#trc_pattern{m=Module,f=Function,a=Arity}]});
trace(_,_,_,_) ->
    {error, badarg}.

-spec trc(string()) -> ok.
trc(Str) when is_list(Str) ->
    %% http://erldocs.com/current/erts/erlang.html?i=1&search=erlang:trace_pa#trace_pattern/3
    %% not really using Flags, with my DBG implementation yet..
    {{M,F,A},MatchSpec,[_Flag]} = redbug_msc:transform(Str),
    cluster_foreach_call({trace, [], [#trc_pattern{m=M,f=F,a=A,ms=MatchSpec}]}).

-spec trace_modules(list( atom() )) -> ok.
trace_modules(Modules) ->
    trace_modules(Modules, [{time, false}, {messages, false}]).

-spec trace_modules(list(), list()) -> ok.
trace_modules(Modules, Opts) ->
    cluster_foreach_call_infinity({trace, Opts, [ #trc_pattern{m=M} || M <- Modules,
        %% TODO: build some safetynet...
        ( M /= erlang orelse
          M /= lists orelse
          M /= io )
    ]}).

-spec stop_trace() -> ok.
stop_trace() ->
    cluster_foreach_call(stop_all_trace_patterns).

-spec stop_trace(atom()) -> ok.
stop_trace(Module) ->
    cluster_foreach_call({stop_trace, #trc_pattern{m=Module}}).

-spec stop_trace(atom(), atom()) -> ok.
stop_trace(Module, Function) ->
    cluster_foreach_call({stop_trace, #trc_pattern{m=Module,f=Function}}).

-spec stop_trace(atom(), atom(), integer()) -> ok.
stop_trace(Module, Function, Arity) ->
    cluster_foreach_call({stop_trace, #trc_pattern{m=Module,f=Function,a=Arity}}).

%%------------------------------------------------------------------------
-spec clear_all_traces() -> ok.
clear_all_traces() ->
    cluster_foreach_call({clear_db_traces}).

cluster_foreach_call(Msg) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            call_node(Node, Cookie, Msg)
        end, ets:tab2list(nodelist)
    ).

cluster_foreach_call_infinity(Msg) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            call_node_infinity(Node, Cookie, Msg)
        end, ets:tab2list(nodelist)
    ).

%% TODO: HANDLE TIMEOUT,
%% TODO: make default infinity, and top level has timeouts.....
call_node(ChildId, Msg) ->
    gen_server:call(ChildId, Msg).
call_node(Node, Cookie, Msg) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    call_node(ChildId, Msg).

call_node_infinity(Node, Cookie, Msg) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    gen_server:call(ChildId, Msg, infinity).
%%------------------------------------------------------------------------
-spec recv_trace(list(), end_of_trace | goanna_forward_callback_mod:erlang_trace_data()) -> ok | stop_tracing.
recv_trace([trace, _ChildId], end_of_trace) ->
    ok;
recv_trace([trace, ChildId], Trace={trace, _Pid, _Label, _Info}) ->
    call_node(ChildId, {trace_item, Trace});
recv_trace([trace, ChildId], Trace={trace, _Pid, _Label, _Info, _Extra}) ->
    call_node(ChildId, {trace_item, Trace});
recv_trace([trace, ChildId], Trace={trace_ts, _Pid, _Label, _Info, _ReportedTS}) ->
    call_node(ChildId, {trace_item, Trace});
recv_trace([trace, ChildId], Trace={trace_ts, _Pid, _Label, _Info, _Extra, _ReportedTS}) ->
    call_node(ChildId, {trace_item, Trace});
recv_trace([trace, _ChildId], Trace={seq_trace, _Label, _SeqTraceInfo}) ->
    io:format("! Trace Message ~p not implemented yet !", [Trace]);
recv_trace([trace, _ChildId], Trace={seq_trace, _Label, _SeqTraceInfo, _ReportedTS}) ->
    io:format("! Trace Message ~p not implemented yet !", [Trace]);
recv_trace([trace, _ChildId], _Trace={drop, NumberOfDroppedItems}) ->
    io:format("! Remote DBG dropped ~p message. Goanna could not consume fast enough!", [NumberOfDroppedItems]);
recv_trace(_, _) ->
    ok.
-spec list_active_traces() -> list().
list_active_traces() ->
    ets:tab2list(tracelist).

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