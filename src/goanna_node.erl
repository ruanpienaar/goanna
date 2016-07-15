-module(goanna_node).

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
-record(?STATE, {child_id,
                 connected=false,
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
                 active_trace_patterns=orddict:new(),
                 trace_active=false
                }).
%%------------------------------------------------------------------------
start_link({Node, Cookie, Type}) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    gen_server:start_link({local, ChildId}, ?MODULE, {Node,Cookie,Type, ChildId}, []).

init({Node, Cookie, Type, ChildId}) ->
    {ok, _NodeObj} = goanna_db:init_node([Node, Cookie, Type]),
    false = process_flag(trap_exit, true),
    DefaultTraceOpts = application:get_env(goanna, default_trace_options, []),
    TraceTime = default_or_new_option(time, DefaultTraceOpts, false),
    MessageCount = default_or_new_option(messages, DefaultTraceOpts, false),
    {ok, Mod} = application:get_env(goanna, forward_callback_mod),
    check_forward_mod(Mod),
    est_rem_conn(
        #?STATE{child_id=ChildId,
                node=Node,
                cookie=Cookie,
                type=Type,
                forward_callback_mod=Mod,
                trace_msg_total=MessageCount,
                trace_time=TraceTime
    }).

handle_call({update_state}, _From, State) ->
    DefaultTraceOpts = application:get_env(goanna, default_trace_options, []),
    TraceTime = default_or_new_option(time, DefaultTraceOpts, false),
    MessageCount = default_or_new_option(messages, DefaultTraceOpts, false),
    {reply, ok, State#?STATE{trace_msg_total=MessageCount,
                             trace_time=TraceTime}};
%%------------------------------------------------------------------------
%%---Tracing--------------------------------------------------------------
handle_call({trace, Opts}, _From, #?STATE{node=Node,
                                          active_trace_patterns=ATP,
                                          child_id=ChildId,
                                          trace_time = TT,
                                          trace_timer_tref = TTT} = State) ->
    {trc, TrcPattern} = lists:keyfind(trc, 1, Opts),
    MSecTime = default_or_new_option(time, Opts, TT),
    NewMaxMsgTotal = default_or_new_option(messages, Opts, State#?STATE.trace_msg_total),
    ?CRITICAL("TRACING for ~p seconds, and ~p messages", [MSecTime, NewMaxMsgTotal]),
    case { goanna_db:lookup([trc_pattern, ChildId, TrcPattern]),
           orddict:find(TrcPattern, ATP)
         }
    of
        %% Neither db nor state has entry
        { [],
          error
        } ->
            true = goanna_db:store([tracelist, ChildId], [TrcPattern, Opts]),
            enable_tracing(Node, TrcPattern),
            {reply, ok, State#?STATE{
                active_trace_patterns = orddict:append(TrcPattern, nothing_yet, ATP),
                trace_msg_total = NewMaxMsgTotal,
                trace_timer_tref = trace_timer(MSecTime, TT, TTT, TrcPattern, ChildId),
                trace_active=true
            }};
        %% db has entry, state doesn't
        { [{{ChildId, TrcPattern}, CreatedTimestamp}],
          error
        } ->
            true = goanna_db:store([tracelist, ChildId], [TrcPattern, Opts]),
            enable_tracing(Node, TrcPattern),
            {reply, ok, State#?STATE{
                active_trace_patterns = orddict:append(TrcPattern, nothing_yet, ATP),
                trace_msg_total = NewMaxMsgTotal,
                trace_timer_tref = trace_timer(MSecTime, TT, TTT, TrcPattern, ChildId),
                trace_active=true
            }};

        %% db has no entry, state had entry
        { [],
          {ok,[nothing_yet]}
        } ->
            true = goanna_db:store([tracelist, ChildId], [TrcPattern, Opts]),
            {noreply, ok, State#?STATE{
                trace_msg_total = NewMaxMsgTotal,
                trace_timer_tref = trace_timer(MSecTime, TT, TTT, TrcPattern, ChildId),
                trace_active=true
            }};
        %% Both db and state has entry
        { [{{ChildId, TrcPattern}, CreatedTimestamp}],
          {ok,[nothing_yet]}
        } ->
            %% ReInsert db entry, with potentially new OPTS...
            true = goanna_db:store([tracelist, ChildId], [TrcPattern, Opts]),
            {noreply, ok, State#?STATE{
                trace_msg_total = NewMaxMsgTotal,
                trace_timer_tref = trace_timer(MSecTime, TT, TTT, TrcPattern, ChildId),
                trace_active=true
            }}
    end;
%%------------------------------------------------------------------------
%%---Deal with message counts---------------------------------------------
%% TODO: handle: {trace_item,end_of_trace}
handle_call({trace_item, _}, _From, #?STATE{ % node = Node,
											 % cookie = Cookie,
											 trace_active=false } = State) ->
    % io:format("."),
    % disable_all_tracing(Node, Cookie),
    % cancel_timer(State#?STATE.trace_timer_tref),
    {reply, ok, State};
handle_call({trace_item, Trace}, _From, #?STATE{ trace_msg_count=TMC,
                                                 trace_msg_total=false,
                                                 trace_active=true } = State) ->

    %% Either use the external timestamp, as the db key, or create your own one...
    %% trace VS trace_ts
    % case Trace of 
    %     trace ->
    %         ok;
    %     trace_ts ->
    %         ok
    % end,

    true=goanna_db:store([trace, State#?STATE.child_id], Trace),
    {reply, ok, State#?STATE{trace_msg_count = TMC + 1}};
handle_call({trace_item, Trace}, _From, #?STATE{ trace_msg_count=TMC,
                                                 trace_msg_total=TMT,
                                                 trace_active=true } = State) when TMC<TMT ->
    true=goanna_db:store([trace, State#?STATE.child_id], Trace),
    {reply, ok, State#?STATE{trace_msg_count = TMC + 1}};
handle_call({trace_item, _}, _From, #?STATE{ node=Node,
                                             cookie=Cookie,
                                             trace_msg_count=TMC,
                                             trace_msg_total=TMT,
                                             trace_active=true } = State) when TMC>=TMT ->
    ?INFO("[~p] [~p] Trace message count limit reached...", [?MODULE, Node]),
    disable_all_tracing(Node, Cookie),
    cancel_timer(State#?STATE.trace_timer_tref),
    {reply, ok, State#?STATE{trace_msg_count = 0,
                             active_trace_patterns = orddict:new(),
                             trace_timer_tref = false,
                             trace_active = false
    }};
handle_call(stop_all_trace_patterns,
        _From, #?STATE{node=Node, cookie=Cookie} = State) ->

    disable_all_tracing(Node, Cookie),
    % cancel_timer(State#?STATE.trace_timer_tref),

    {reply, ok, State#?STATE{
        active_trace_patterns = orddict:new(),
        trace_msg_count = 0,
        trace_timer_tref = false
    }};
handle_call({stop_trace, TrcPattern},
        _From, #?STATE{node=Node, cookie=Cookie, active_trace_patterns=ATP, trace_msg_count=TMC} = State) ->
    ?INFO("TMC:~p~n", [TMC]),
    disable_tracing(Node, TrcPattern),
    cancel_timer(State#?STATE.trace_timer_tref),
    goanna_db:truncate_tracelist([Node, Cookie]),
    {reply, ok, State#?STATE{
        active_trace_patterns = orddict:erase(TrcPattern, ATP),
        trace_msg_count = 0,
        trace_timer_tref = false
    }};
handle_call(Request, _From, State) ->
    ?EMERGENCY("Unknown handle_call REQUEST: ~p", [Request]),
    ?EMERGENCY("Unknown handle_call STATE: ~p", [State]),
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
%%---Poll results, and forward upwards------------------------------------
handle_info({push_data}, #?STATE{node=Node, cookie=Cookie, forward_callback_mod=Mod } = State) ->
	ok = push_data(Mod, Node, Cookie),
    forwarding(),
	{noreply, State};
%%------------------------------------------------------------------------
%% Handle Exists
handle_info({'EXIT', From, done}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) when From == self() ->
    ?EMERGENCY("EXIT - ~p - ~p", [From, Reason]),
    ?EMERGENCY("~p", [erlang:process_info(From)]),
    {stop, Reason, State};
handle_info({'EXIT', From, Reason}, State) ->
    % [alert] <0.188.0> sent EXIT normal
    % Some of these normals, are the port from dbg.
    % when using type==tcpip_port, dbg creates 2 important items,
    % 1) the dbg loop pid 2) the port for the tcp traffic, this second
    % items creates the normal crash, when you stop-start dbg.
    ?ALERT("~p sent EXIT ~p", [From, Reason]),
    {noreply, State};
%%------------------------------------------------------------------------
%%---Shoudn't happen, but hey... let's see ...----------------------------
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?ALERT("[~p] Unknown handle_info(~p, ...) in ~p", [?MODULE, Info, goanna_node_sup:id(Node, Cookie)]),
    {noreply, State}.

terminate(Reason, #?STATE{ node=Node, cookie=Cookie } = _State) ->
    ?CRITICAL("[~p] terminate ~p in ~p", [?MODULE, Reason, goanna_node_sup:id(Node, Cookie)]),
    disable_all_tracing(Node, Cookie),
    true = goanna_db:delete_node(Node),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%------------------------------------------------------------------------
est_rem_conn(#?STATE{ node=Node, cookie=Cookie, type=Type, child_id=ChildId } = State) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            do_monitor_node(Node, State#?STATE.connect_attempt_ref),
            trace_steps(Node, Cookie, Type),
            forwarding(),
            reapply_traces(Node, Cookie),
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
%%------------------------------------------------------------------------
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
    %% Dbg pid is already linked...
    ?DEBUG("DBG START Process:~p~n", [RemoteDbgPid]),
    PortGenerator = rpc:call(Node, dbg, trace_port, [ip, RelayPort]),
    case rpc:call(Node, dbg, tracer, [port, PortGenerator]) of
        {error, already_started} ->
            ok;
        {ok, RemoteDbgPid} ->
            ok
    end,
    {ok,MatchDesc} = dbg_p(Node),
    {ok, Fun} = handler_fun(Node, Cookie, tcpip_port),
    CLientPid = dbg:trace_client(ip, {RelayHost, RelayPort}, {Fun, ok}),
    link(CLientPid),
    ?INFO("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
    RemoteDbgPid.

erlang_distribution_trace_steps(Node, Cookie) ->
    {ok, RemoteDbgPid} = dbg_start(Node),
    ?DEBUG("DBG START Process:~n~p", [RemoteDbgPid]),
    {ok, RemoteFun} = handler_fun(Node, Cookie, erlang_distribution),
    case rpc:call(Node, dbg, tracer,
                 [Node, process, {RemoteFun, ok}])
    of
        {error, already_started} ->
            ok;
        {ok, Node} ->
            ok
    end,
    {ok,MatchDesc} = dbg_p(Node),
    ?INFO("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
    RemoteDbgPid.

dbg_p(Node) ->
    Flags = application:get_env(goanna, dbg_p_flags, call),
    {ok,_MatchDesc} = rpc:call(Node, dbg, p, [all, Flags]).

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
        ok=goanna_api:recv_trace([trace, goanna_node_sup:id(Node,Cookie)],Trace)
    end};
handler_fun(_Node, _Cookie, file) ->
    {ok, undefined};
handler_fun(Node, Cookie, erlang_distribution) ->
    FunStr = lists:flatten(io_lib:format(
        "fun(Trace, _) ->"
        " ok=rpc:call(~p, goanna_api, recv_trace, [[trace,~p],Trace]) "
        "end.", [node(), goanna_node_sup:id(Node,Cookie)])),
    {ok, Tokens, _} = erl_scan:string(FunStr),
    {ok,[Form]} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:add_binding('B', 2, erl_eval:new_bindings()),
    {value, Fun, _} = erl_eval:expr(Form, Bindings),
    {ok, Fun}.
%%------------------------------------------------------------------------
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
%%------------------------------------------------------------------------
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
%%------------------------------------------------------------------------
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
%%------------------------------------------------------------------------
%% TODO: add a batch size here...
push_data(Mod, Node, Cookie) ->
	Tbl=goanna_node_sup:id(Node, Cookie),
	trace_results_loop(Mod, Tbl, Node).

forwarding() ->
    case application:get_env(goanna, push_interval) of
        undefined ->
            ok;
        {ok, Interval} ->
            erlang:send_after(Interval, self(), {push_data})
    end.
%%------------------------------------------------------------------------
trace_results_loop(Mod, Tbl, Node) ->
    First = 
        try
            ets:first(Tbl)
        catch
            C:E ->
                ?CRITICAL("ets:first got exception ~p ~p", [C, E]),
                '$end_of_table'
        end,
    trace_results_loop(Mod, Tbl, Node, First).

%% TODO: Simplify, and abstract out, this simple db table walk
trace_results_loop(_Mod, _Tbl, _Node, '$end_of_table') ->
    ok;
trace_results_loop(Mod, Tbl, Node, Key) ->
    [E] = goanna_db:lookup([trace, Tbl, Key]),
    ok = Mod:forward(Tbl, E),
    true = ets:delete(Tbl, Key),
    trace_results_loop(Mod, Tbl, Node, ets:next(Tbl, Key)).
%%------------------------------------------------------------------------
disable_all_tracing(Node, Cookie) ->
    disable_tracing(Node, []),
    rpc:call(Node, dbg, stop_clear, []),
    %% Just make sure dbg, and tracer is always started..
    {ok, _RemoteDbgPid} = dbg_start(Node),
    [{Node, Cookie, Type}] = goanna_db:lookup([nodelist, Node]),
    trace_steps(Node, Cookie, Type).
%%------------------------------------------------------------------------
trace_timer(MSecTime, DefaultMSecTime, ExistingTRef, TrcPattern, ChildId) ->
    cancel_timer(ExistingTRef),
    Time =
        case {MSecTime, DefaultMSecTime} of
            {false,false} -> false;
            {false,T}     -> T;
            {T,false}     -> T;
            {T,_}         -> T
        end,
    stop_trace_event(Time, TrcPattern, ChildId).

cancel_timer(ExistingTRef) ->
    case ExistingTRef of
        false        -> ok;
        ExistingTRef -> timer:cancel(ExistingTRef)
    end.

stop_trace_event(false, _TrcPattern, _ChildId) ->
    false;
stop_trace_event(Time, TrcPattern, ChildId) ->
    ?INFO("STOPPING traces after ~p seconds", [Time/1000]),
    {ok, TRef} = timer:apply_after(Time, gen_server, call, [ChildId, {stop_trace, TrcPattern}]),
    TRef.
%%------------------------------------------------------------------------
default_or_new_option(Field, Opts, Default) ->
    case lists:keyfind(Field, 1, Opts) of
        false          -> Default;
        {Field, Value} -> Value
    end.

reapply_traces(Node, Cookie) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    ?INFO("ReApplyging traces on remote node ~p", [Node]),
    TracePatterns = [ {{C,TP}, Timestamp, Options} || {{C,TP}, Timestamp, Options} <- ets:tab2list(tracelist), ChildId == C ],
    [ begin
        ?DEBUG("tracing - ~p", [TP]),
        enable_tracing(Node, TP)
      end || {{_C,TP}, Timestamp, Opts} <- TracePatterns ].

check_forward_mod(undefined) ->
    ok;
check_forward_mod(Mod) ->
    % Make sure the default is loaded...ugly...
    c:l(Mod),

    % TODO: build a behaviour checker...
    case erlang:function_exported(Mod, forward, 2) of
        true ->
            ok;
        false ->
            ?CRITICAL("[~p] forward_callback_mod ~p missing required behaviour functions",
                [?MODULE, Mod]),
            erlang:halt(1),
            ok
    end.