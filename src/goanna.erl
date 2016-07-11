-module (goanna).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("goanna.hrl").

-define(STATE, goanna_state).
-record(?STATE, {connected=false,
                 connect_attempt_ref=undefined,
                 connect_attemps=0,
                 node,
                 cookie,
                 type,
                 forward_callback_mod,
                 trace_msg_count=0,
                 trace_msg_total,
                 trace_time,
                 trace_timer_tref=false,
                 active_trace_patterns=orddict:new()
                }).
%%------------------------------------------------------------------------
start_link({Node, Cookie, Type}) ->
    Name = goanna_sup:id(Node, Cookie),
    gen_server:start_link({local, Name}, ?MODULE, {Node,Cookie,Type}, []).

init({Node, Cookie, Type}) ->
    {ok, Mod} = application:get_env(goanna, forward_callback_mod),
    DefaultTraceOpts = application:get_env(goanna, default_trace_options, []),
    TraceTime = default_or_new_option(time, DefaultTraceOpts, false),
    MessageCount = lists:keyfind(messages, 1, DefaultTraceOpts),
    case Mod of
        undefined ->
            ok;
        Mod ->
            % Make sure the default is loaded...ugly...
            c:l(goanna_shell_printer),

            % TODO: build a behaviour checker...
            case erlang:function_exported(Mod, forward, 2) of
                true ->
                    ok;
                false ->
                    ?CRITICAL("[~p] forward_callback_mod ~p missing required behaviour functions",
                        [?MODULE, Mod]),
                    erlang:halt(1),
                    ok
            end
    end,
    est_rem_conn(
        #?STATE{node=Node,
                cookie=Cookie,
                type=Type,
                forward_callback_mod=Mod,
                trace_msg_total=MessageCount,
                trace_time=TraceTime
    }).

%%------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------
%% Connectivity-----------------------------------------------------------
handle_info({nodedown, Node}, #?STATE{node=Node} = State) ->
    ?ALERT("[~p] Node:~p down...", [?MODULE, Node]),
    {noreply, reconnect(State)};
handle_info(reconnect, #?STATE{node=Node} = State) ->
    ?DEBUG("[~p] attempting to reconnect to ~p...", [?MODULE, Node]),
    {ok, NewState} = est_rem_conn(State),
    {noreply, NewState};
%%------------------------------------------------------------------------
%%---Disable Tracing------------------------------------------------------
handle_info(stop_all_trace_patterns,#?STATE{node=Node, cookie=Cookie} = State) ->
    disable_all_tracing(Node, Cookie),
    {noreply, State#?STATE{active_trace_patterns = orddict:new()}};
handle_info({stop_trace, TrcPattern},
        #?STATE{node=Node, active_trace_patterns=ATP} = State) ->
    disable_tracing(Node, TrcPattern),
    {noreply, State#?STATE{
        active_trace_patterns = orddict:erase(TrcPattern, ATP)
    }};
%%------------------------------------------------------------------------
%%---Tracing--------------------------------------------------------------
handle_info({trace, Opts}, #?STATE{node=Node, active_trace_patterns=ATP } = State) ->
    {trc, TrcPattern} = lists:keyfind(trc, 1, Opts),
    MSecTime = default_or_new_option(time, Opts, State#?STATE.trace_time),
    NewMaxMsgCount = default_or_new_option(messages, Opts, State#?STATE.trace_msg_total),
    case orddict:find(TrcPattern, ATP) of
        error ->
            enable_tracing(Node, TrcPattern),
            {noreply, State#?STATE{
                active_trace_patterns = orddict:append(TrcPattern, nothing_yet, ATP),
                trace_msg_count = NewMaxMsgCount,
                trace_timer_tref =
                    trace_timer(MSecTime, State#?STATE.trace_time, State#?STATE.trace_timer_tref, TrcPattern)
            }};
        %% Just re-new the timer, if called with the same pattern
        {ok,nothing_yet} ->
            {noreply, State#?STATE{
                trace_msg_count = NewMaxMsgCount,
                trace_timer_tref =
                    trace_timer(MSecTime, State#?STATE.trace_time, State#?STATE.trace_timer_tref, TrcPattern)
            }}
    end;
%%------------------------------------------------------------------------
%%---Poll results, and forward upwards------------------------------------
handle_info({push_data}, #?STATE{node=Node, cookie=Cookie, forward_callback_mod=Mod } = State) ->
	ok = push_data(Mod, Node, Cookie),
    forwarding(),
	{noreply, State};
%%------------------------------------------------------------------------
%%---Deal with message counts---------------------------------------------
handle_info({trace_item}, #?STATE{ trace_msg_count=TMC, trace_msg_total=TMT } = State) when TMC<TMT ->
    {noreply, State#?STATE{trace_msg_count = TMC + 1}};
handle_info({trace_item}, #?STATE{ node=Node, cookie=Cookie,
                                   trace_msg_count=TMC, trace_msg_total=TMT } = State) when TMC>=TMT ->
    ?INFO("[~p] [~p] Trace message count limit reached...", [?MODULE, Node]),
    disable_all_tracing(Node, Cookie),
    {noreply, State#?STATE{trace_msg_count = 0,
                           active_trace_patterns = orddict:new()}};
%%------------------------------------------------------------------------
%%---Shoudn't happen, but hey... let's see ...----------------------------
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?ALERT("[~p] Unknown handle_info(~p, ...) in ~p", [?MODULE, Info, goanna_sup:id(Node, Cookie)]),
    {noreply, State}.

terminate(Reason, #?STATE{ node=Node, cookie=Cookie } = _State) ->
    ?CRITICAL("[~p] terminate ~p in ~p", [?MODULE, Reason, goanna_sup:id(Node, Cookie)]),
    true = goanna_db:delete_node(Node),
    disable_all_tracing(Node, Cookie),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------
est_rem_conn(#?STATE{ node=Node, cookie=Cookie, type=Type } = State) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            do_monitor_node(Node, State#?STATE.connect_attempt_ref),
            trace_steps(Node, Cookie, Type),
            forwarding(),
            {ok, State#?STATE{connected=true,
                              connect_attempt_ref = undefined,
                              connect_attemps = 0}};
        false ->
            {ok, reconnect(State)}
    end.

do_rem_conn(Node, Cookie) ->
    ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))).

do_monitor_node(Node, ConnectAttemptTref) ->
    true = erlang:monitor_node(Node, true),
    case ConnectAttemptTref of
        undefined -> ok;
        TRef      -> {ok, cancel} = timer:cancel(TRef)
    end,
    ?INFO("[~p] ----------------- NODE ~p STARTUP COMPLETED -----------------\n\n",
        [?MODULE, Node]).

trace_steps(Node, Cookie, tcpip_port) ->
    tcpip_port_trace_steps(Node, Cookie);
trace_steps(_Node, _Cookie, file) ->
    ?WARNING("dbg trace_port file - fearure not implemented..."),
    erlang:halt(1);
trace_steps(Node, Cookie, erlang_distribution) ->
    erlang_distribution_trace_steps(Node, Cookie).

tcpip_port_trace_steps(Node, Cookie) ->
    RelayPort = erlang:phash2(Node, 9000)+1023, % +1023, to make sure it's above the safe range
    [_Name, RelayHost] = string:tokens(atom_to_list(Node), "@"),
    {ok, RemoteDbgPid} = dbg_start(Node),
    PortGenerator = rpc:call(Node, dbg, trace_port, [ip, RelayPort]),
    case rpc:call(Node, dbg, tracer, [port, PortGenerator]) of
        {error,already_started} ->
            ok;
        {ok, RemoteDbgPid} ->
            ok
    end,
    {ok,MatchDesc} = rpc:call(Node, dbg, p, [all, call]),
    {ok, Fun} = handler_fun(Node, Cookie, tcpip_port),
    CLientPid = dbg:trace_client(ip, {RelayHost, RelayPort}, {Fun, ok}),
    link(CLientPid),
    ?INFO("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]).

erlang_distribution_trace_steps(Node, Cookie) ->
    {ok, _RemoteDbgPid} = dbg_start(Node),
    {ok, RemoteFun} = handler_fun(Node, Cookie, erlang_distribution),
    case rpc:call(Node, dbg, tracer,
                 [Node, process, {RemoteFun, ok}])
    of
        {error,already_started} ->
            ok;
        {ok, Node} ->
            ok
    end,
    {ok,MatchDesc} = rpc:call(Node, dbg, p, [all, call]),
    ?INFO("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]).

dbg_start(Node) ->
    try
        {ok, _Pid} = rpc:call(Node, dbg, start, [])
    catch
        error:{badmatch,{badrpc,{'EXIT',
                {{case_clause,DbgPid},_}
              }}} ->
            {ok, DbgPid};
        C:E ->
            ?WARNING("[~p] dbg start failed ~p", [?MODULE, {C,E}]),
            {ok, undefined}
    end.

handler_fun(Node, Cookie, tcpip_port) ->
    {ok, fun(Trace, _) ->
        goanna_api:store_trace([trace, Node, Cookie],Trace)
    end};
handler_fun(_Node, _Cookie, file) ->
    {ok, undefined};
handler_fun(Node, Cookie, erlang_distribution) ->
    FunStr = lists:flatten(io_lib:format(
        "fun(Trace, _) ->"
        " true=rpc:call(~p, goanna_api, store_trace, [[trace,~p,~p],Trace]) "
        "end.", [node(), Node, Cookie])),
    {ok, Tokens, _} = erl_scan:string(FunStr),
    {ok,[Form]} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:add_binding('B', 2, erl_eval:new_bindings()),
    {value, Fun, _} = erl_eval:expr(Form, Bindings),
    {ok, Fun}.

%% TODO: maybe delete this child, when it cannot establish a connection
%% after X amount of retries...
reconnect(#?STATE{ connect_attemps = Attempts } = State) when Attempts > 10 ->
    TRef = erlang:send_after(1000, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef, connect_attemps = Attempts+1 };
reconnect(#?STATE{ connect_attemps = Attempts } = State) when Attempts > 3 ->
    TRef = erlang:send_after(250, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef, connect_attemps = Attempts+1 };
reconnect(#?STATE{ connect_attemps = Attempts } = State) when Attempts =< 3 ->
    TRef = erlang:send_after(50, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef, connect_attemps = Attempts+1 }.

enable_tracing(_Node, false) ->
    {error, nothing_to_trace};
enable_tracing(Node, T=#trc_pattern{m=Module,f=undefined,a=undefined}) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
enable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=undefined}) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
enable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=Arity}) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl,[Module, Function, Arity, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]).

disable_tracing(Node, []) ->
    ?INFO("[~p] [~p] Disable all tracing", [?MODULE, Node]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, []),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
disable_tracing(Node, T=#trc_pattern{m=Module,f=undefined,a=undefined}) ->
    ?INFO("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module]),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
disable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=undefined}) ->
    ?INFO("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module, Function]),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
disable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=Arity}) ->
    ?INFO("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module, Function, Arity]),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]).

push_data(Mod, Node, Cookie) ->
	Tbl=goanna_sup:id(Node, Cookie),
	trace_results_loop(Mod, Tbl, Node).

forwarding() ->
    case application:get_env(goanna, push_interval) of
        undefined ->
            ok;
        {ok, Interval} ->
            erlang:send_after(Interval, self(), {push_data})
    end.

trace_results_loop(Mod, Tbl, Node) ->
    trace_results_loop(Mod, Tbl, Node, ets:first(Tbl)).

%% TODO: Simplify, and abstract out, this simple db table walk
trace_results_loop(_Mod, _Tbl, _Node, '$end_of_table') ->
    ok;
trace_results_loop(Mod, Tbl, Node, Key) ->
    ok = Mod:forward(Tbl, goanna_db:lookup([trace, Tbl, Key])),
    true = ets:delete(Tbl, Key),
    trace_results_loop(Mod, Tbl, Node, ets:next(Tbl, Key)).

disable_all_tracing(Node, Cookie) ->
    ok = rpc:call(Node, dbg, stop_clear, []),
    %% Just make sure dbg, and tracer is always started..
    {ok, _RemoteDbgPid} = dbg_start(Node),
    [{Node, Cookie, Type}] = goanna_db:lookup([nodelist, Node]),
    trace_steps(Node, Cookie, Type),
    disable_tracing(Node, []).

trace_timer(MSecTime, DefaultMSecTime, ExistingTRef, TrcPattern) ->
    case ExistingTRef of
        undefined    -> ok;
        ExistingTRef -> {ok, cancel} = timer:cancel(ExistingTRef)
    end,
    Time =
        case {MSecTime, DefaultMSecTime} of
            {false,false} -> false;
            {false,T}     -> T;
            {T,false}     -> T;
            {T,_}         -> T
        end,
    stop_trace_event(Time, TrcPattern).

stop_trace_event(false, TrcPattern) ->
    ok;
stop_trace_event(Time, TrcPattern) ->
    TRef = erlang:send_after(Time, self(), {stop_trace, TrcPattern}).

default_or_new_option(Field, Opts, Default) ->
    case lists:keyfind(Field, 1, Opts) of
        false          -> Default;
        {Field, Value} -> Value
    end.