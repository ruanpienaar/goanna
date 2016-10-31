-module(goanna_node).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1, init/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("goanna.hrl").
-define(STATE, ?GOANNA_STATE).
%%------------------------------------------------------------------------
start_link({Node, Cookie, Type}) ->
    ChildId = goanna_node_sup:id(Node, Cookie),
    gen_server:start_link({local, ChildId}, ?MODULE, {Node,Cookie,Type, ChildId}, []).

init({Node, Cookie, Type, ChildId}, sync) ->
    %% First Connect, then allow initialisation to continue...
    %% follow the normal connect attempt route...
    ok.

init({Node, Cookie, Type, ChildId}) ->
    false = process_flag(trap_exit, true),
    est_rem_conn(app_env_to_state(#?STATE{child_id=ChildId, node=Node, cookie=Cookie, type=Type})).
%%------------------------------------------------------------------------
%%---For updating the internal state of this gs---------------------------
-spec app_env_to_state( #?STATE{} ) -> #?STATE{}.
app_env_to_state(State) ->
    %% TODO: add some app env checks here...
    DefaultTraceOpts = application:get_env(goanna, default_trace_options, []),
    FMethod =
        case application:get_env(goanna, data_retrival_method, pull) of
            F={push, _, M} when M =/= undefined ->
                ok = check_forward_mod(M),
                F;
            pull ->
                pull
        end,
    TraceTime = list_property_or_default(time, DefaultTraceOpts, false),
    MessageCount = list_property_or_default(messages, DefaultTraceOpts, false),
    MaxConnAttempts = application:get_env(goanna, max_reconnecion_attempts, undefined),
    State#?STATE{trace_msg_total=MessageCount,
                 trace_time=TraceTime,
                 data_retrival_method=FMethod,
                 max_reconnecion_attempts=MaxConnAttempts
    }.

-spec est_rem_conn(#?STATE{}) -> {ok, #?STATE{}}.
est_rem_conn(#?STATE{node=Node,
                     cookie=Cookie,
                     data_retrival_method=FMethod } = State) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            ok = do_monitor_node(Node, State#?STATE.connect_attempt_ref),
            {ok, _} = trace_steps(Node, Cookie, State#?STATE.type),
            {ok, reapply_traces(State#?STATE{
                    connected=true,
                    connect_attempt_ref = undefined,
                    connect_attempts = 0,
                    push_pending = handle_data_retrival_method(FMethod)
                })
            };
        false ->
            {ok, reconnect(State)}
    end.

do_rem_conn(Node, Cookie) ->
    ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect_node(Node))).

do_monitor_node(Node, ConnectAttemptTref) ->
    true = erlang:monitor_node(Node, true),
    ?NOTICE(" [~p] ~p !!! startup complete !!! \n", [?MODULE, Node]),
    case ConnectAttemptTref of
        undefined ->
            ok;
        TRef ->
	    	case timer:cancel(TRef) of
	        %% Most likely already closed/timed-out...
	        	{error, badarg} ->
	            	ok;
	            {ok, cancel} ->
	            	ok
	        end
    end.


trace(_ChildId, [], _, _) ->
    ok;
trace(ChildId, [H|T], UpdatedOpts2, Node) ->
    %% TODO: how to report, already traced...
    _ = trace(ChildId, H, UpdatedOpts2, Node),
    trace(ChildId, T, UpdatedOpts2, Node);
trace(ChildId, TrcPattern, UpdatedOpts2, Node) ->
    case goanna_db:lookup([trc_pattern, ChildId, TrcPattern]) of
        [] ->
            true = goanna_db:store([tracelist, ChildId, TrcPattern], UpdatedOpts2),
            enable_tracing(Node, TrcPattern);
        [{{ChildId, TrcPattern}, _CreatedDatetime, _Opts}] ->
            {error, {TrcPattern, already_tracing}}
    end.

%%------------------------------------------------------------------------
%%--- Updating the internal state, based on app_env-----------------------
handle_call({update_state}, _From, State) ->
    {reply, ok, app_env_to_state(State)};
%%------------------------------------------------------------------------
%%---Tracing--------------------------------------------------------------
handle_call({trace, Opts, TrcPatterns}, _From, #?STATE{child_id=ChildId,
                                                      node=Node,
                                                      trace_msg_total=TraceMsgCount,
                                                      trace_time=TraceTime,
                                                      trace_timer_tref=TraceTimerTRef } = State) ->
    NewTraceMsgCount = list_property_or_default(messages, Opts, TraceMsgCount),
    NewTraceTime = list_property_or_default(time, Opts, TraceTime),
    UpdatedOpts = lists:keystore(messages, 1, Opts, {messages, NewTraceMsgCount}),
    UpdatedOpts2 = lists:keystore(time, 1, UpdatedOpts, {time, NewTraceTime}),

    %% TODO: Handle already traced...
    ok = trace(ChildId, TrcPatterns, UpdatedOpts2, Node),
    {ok,_} = dbg_p(Node),
    {reply, ok,
        State#?STATE{
            trace_msg_total=NewTraceMsgCount,
            trace_time=NewTraceTime,
            trace_timer_tref = new_trace_timer(ChildId, NewTraceTime, TraceTimerTRef),
            trace_active = true
        }
    };
%%------------------------------------------------------------------------
%%---Deal with message counts---------------------------------------------

handle_call({trace_item, _}, _From, #?STATE{ trace_active=false } = State) ->
    {reply, ok, State};
handle_call({trace_item, Trace}, _From, #?STATE{ trace_msg_count=TMC,
                                                 trace_msg_total=false,
                                                 trace_active=true } = State) ->
    %% Either use the external timestamp, as the db key, or create your own one...
    %% trace VS trace_ts
    % case Trace of
    %     trace -> ok;
    %     trace_ts -> ok
    % end,
    true=goanna_db:store([trace, State#?STATE.child_id], Trace),
    {reply, ok, State#?STATE{trace_msg_count = TMC + 1}};
handle_call({trace_item, Trace}, _From, #?STATE{ trace_msg_count=TMC,
                                                 trace_msg_total=TMT,
                                                 trace_active=true } = State) when TMC<TMT ->
    true=goanna_db:store([trace, State#?STATE.child_id], Trace),
    {reply, ok, State#?STATE{trace_msg_count = TMC + 1}};
handle_call({trace_item, Trace}, _From, #?STATE{ node=Node,
                                                 cookie=Cookie,
                                                 trace_msg_count=TMC,
                                                 trace_msg_total=TMT,
                                                 trace_active=true,
                                                 connected=Connected } = State) when TMC >= TMT ->
    % ?DEBUG("[~p] [~p] Trace message count limit reached...", [?MODULE, Node]),
    true=goanna_db:store([trace, State#?STATE.child_id], Trace),
    {ok,_} = disable_all_tracing(Connected, Node, Cookie),
    ok = cancel_timer(State#?STATE.trace_timer_tref),
    {reply, stop_tracing, State#?STATE{trace_msg_count = 0,
                                       trace_timer_tref = false,
                                       trace_active = false
    }};
handle_call({stop_trace, TrcPattern}, _From, #?STATE{ node = Node, cookie = Cookie } = State) ->
    true = goanna_db:delete_child_id_tracelist(Node, Cookie),
    ok = disable_tracing(Node, TrcPattern),
	{reply, ok, State};
handle_call(stop_all_trace_patterns, _From, #?STATE{node=Node,
                                                    cookie=Cookie,
                                                    trace_msg_count=TMC,
                                                    connected=Connected} = State) ->
    %% TODO: why is this crashing?
    ?DEBUG("Stop trace - Total Message Count:~p~n", [TMC]),
    {ok,_} = disable_all_tracing(Connected, Node, Cookie),
    ok = cancel_timer(State#?STATE.trace_timer_tref),
    true = goanna_db:truncate_tracelist([]),
    {reply, ok, State#?STATE{
        trace_msg_count = 0,
        trace_timer_tref = false,
        trace_active = false
    }};
handle_call({clear_db_traces}, _From, #?STATE{child_id=ChildId} = State) ->
    true = goanna_db:truncate_traces(ChildId),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    ?EMERGENCY("[~p] Unknown handle_call ~p~n~p", [?MODULE, Request, State]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    ?EMERGENCY("[~p] Unknown handle_cast ~p~n~p", [?MODULE, Msg, State]),
    {noreply, State}.
%%------------------------------------------------------------------------
%% Connectivity-----------------------------------------------------------
handle_info({nodedown, Node}, #?STATE{node=Node} = State) ->
    ?DEBUG("[~p] Node:~p down...", [?MODULE, Node]),
    {noreply, reconnect(State)};
handle_info(reconnect, #?STATE{node=Node} = State) ->
    ?DEBUG("[~p] attempting to reconnect to ~p...", [?MODULE, Node]),
    {ok, NewState} = est_rem_conn(State),
    {noreply, NewState};
%%------------------------------------------------------------------------
%%---Poll results, and forward upwards------------------------------------
handle_info({push_data, Mod}, #?STATE{node=Node, child_id=Tbl} = State) ->
    push_data_loop(Mod, Tbl, Node),
    {noreply, State#?STATE{push_pending = handle_data_retrival_method(State#?STATE.data_retrival_method)}};
%%------------------------------------------------------------------------
%% Handle Exists
handle_info({'EXIT', _From, done}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) when From == self() ->
    ?EMERGENCY("EXIT FROM - ~p - ~p", [From, Reason]),
    {stop, Reason, State};
handle_info({'EXIT', From, Reason}, State) ->
    % [alert] <0.188.0> sent EXIT normal
    % Some of these normals, are the port from dbg.
    % when using type==tcpip_port, dbg creates 2 important items,
    % 1) the dbg loop pid 2) the port for the tcp traffic, this second
    % items creates the normal crash, when you stop-start dbg.
    ?DEBUG("~p sent EXIT ~p", [From, Reason]),
    {noreply, State};
%%------------------------------------------------------------------------
%%---Shoudn't happen, but hey... let's see ...----------------------------
handle_info(Info, State) ->
    ?EMERGENCY("[~p] Unknown handle_info ~p~n~p", [?MODULE, Info, State]),
    {noreply, State}.

terminate(Reason, #?STATE{ node=Node, cookie=Cookie, connected=Connected } = _State) ->
    ?DEBUG("[~p] terminate ~p in ~p", [?MODULE, Reason, goanna_node_sup:id(Node, Cookie)]),
    {ok,_} = disable_all_tracing(Connected, Node, Cookie),
    true = goanna_db:delete_node(Node),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%------------------------------------------------------------------------
%% TODO: maybe delete this child, when it cannot establish a connection
%% after X amount of retries...
-spec reconnect(#?STATE{}) -> #?STATE{}.
reconnect(#?STATE{ connect_attempts = Attempts, max_reconnecion_attempts = Max } = State) when Attempts >= Max ->
    ?INFO("Too many connection attempts, removing node ~p", [State#?STATE.node]),
    spawn(fun() -> goanna_api:remove_node(State#?STATE.node) end),
    State#?STATE{connected=false};
reconnect(#?STATE{ connect_attempts = Attempts } = State) when Attempts > 10 ->
    TRef = erlang:send_after(1000, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef, connect_attempts = Attempts+1 };
reconnect(#?STATE{ connect_attempts = Attempts } = State) when Attempts > 3 ->
    TRef = erlang:send_after(250, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef, connect_attempts = Attempts+1 };
reconnect(#?STATE{ connect_attempts = Attempts } = State) when Attempts =< 3 ->
    TRef = erlang:send_after(50, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef, connect_attempts = Attempts+1 }.
%%------------------------------------------------------------------------
trace_steps(Node, Cookie, tcpip_port) ->
    tcpip_port_trace_steps(Node, Cookie);
trace_steps(Node, Cookie, file) ->
    file_port_trace_steps(Node, Cookie);
trace_steps(Node, Cookie, erlang_distribution) ->
    erlang_distribution_trace_steps(Node, Cookie).

tcpip_port_trace_steps(Node, Cookie) ->
    RelayPort = erlang:phash2(Node, 9000)+1023, % +1023, to make sure it's above the safe range
    [_Name, RelayHost] = string:tokens(atom_to_list(Node), "@"),
    case dbg_start(Node) of
        {ok, RemoteDbgPid} ->
            %% Dbg pid is already linked...
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
            ?DEBUG("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
            {ok, RemoteDbgPid};
        Error ->
            Error
    end.

file_port_trace_steps(Node, Cookie) ->
    case dbg_start(Node) of
        {ok, RemoteDbgPid} ->
            PortGenerator = rpc:call(Node, dbg, trace_port, [file, "/Users/rp/hd2/code/goanna/tracefile"]),
            case rpc:call(Node, dbg, tracer, [port, PortGenerator]) of
                {error, already_started} ->
                    ok;
                {ok, RemoteDbgPid} ->
                    ok
            end,
            {ok,MatchDesc} = dbg_p(Node),
            {ok, Fun} = handler_fun(Node, Cookie, file),
            CLientPid = dbg:trace_client(file, "/Users/rp/hd2/code/goanna/tracefile", {Fun, ok}),
            link(CLientPid),
            ?DEBUG("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
            {ok, RemoteDbgPid};
        Error ->
            Error
    end.

erlang_distribution_trace_steps(Node, Cookie) ->
    case dbg_start(Node) of
        {ok, RemoteDbgPid} ->
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
            ?DEBUG("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
            {ok, RemoteDbgPid};
        Error ->
            Error
    end.

dbg_p(Node) ->
    Flags = application:get_env(goanna, dbg_p_flags, call),
    {ok,_MatchDesc} = rpc:call(Node, dbg, p, [all, Flags]).

dbg_start(Node) ->
    try
        {ok, _Pid} = rpc:call(Node, dbg, start, [])
    catch
        error:{badmatch,{badrpc,nodedown}} ->
            {badrpc,nodedown};
        error:{badmatch,{badrpc,{'EXIT',
                {{case_clause,DbgPid},_}
              }}} ->
            ?DEBUG("[~p] DBG START Process:~p~n", [?MODULE, DbgPid]),
            {ok, DbgPid};
        C:E ->
            ?ALERT("[~p] DBG START failed ~p", [?MODULE, {C,E}]),
            {ok, undefined}
    end.

%% TODO: implement file..
%% TODO: how would i stop the remote tracing, if goanna, dies... ?
handler_fun(Node, Cookie, tcpip_port) ->
    {ok, fun(Trace, _) ->
        case goanna_api:recv_trace([trace, goanna_node_sup:id(Node,Cookie)],Trace) of
            ok ->
                ok;
            stop_tracing ->
                ok
        end
    end};
handler_fun(Node, Cookie, file) ->
    {ok, fun(Trace, _) ->
        % case goanna_api:recv_trace([trace, goanna_node_sup:id(Node,Cookie)],Trace) of
        %     ok ->
        %         ok;
        %     stop_tracing ->
        %         ok
        % end
        io:format("FILE TRACE: ~p\n", [Trace])
    end};
handler_fun(Node, Cookie, erlang_distribution) ->
    FunStr = lists:flatten(io_lib:format(
        "fun(Trace, _) -> "
        "  case rpc:call(~p, goanna_api, recv_trace, [[trace,~p],Trace]) of "
        "    ok -> ok; "
        "    stop_tracing -> ok "
        "  end "
        "end.", [node(), goanna_node_sup:id(Node,Cookie)])),
    {ok, Tokens, _} = erl_scan:string(FunStr),
    {ok,[Form]} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:add_binding('B', 2, erl_eval:new_bindings()),
    {value, Fun, _} = erl_eval:expr(Form, Bindings),
    {ok, Fun}.
%%------------------------------------------------------------------------
enable_tracing(_Node, false) ->
    {error, nothing_to_trace};
enable_tracing(Node, T=#trc_pattern{m=Module,f=undefined,a=undefined}) ->
    ?DEBUG("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    ?DEBUG("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
enable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=undefined}) ->
    ?DEBUG("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, cx]),
    ?DEBUG("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);
enable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=Arity}) ->
    ?DEBUG("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl,[Module, Function, Arity, cx]),
    ?DEBUG("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]).
%%------------------------------------------------------------------------
disable_all_tracing(Connected, Node, Cookie) when Connected=:=false ->
    {ok, undefined};
disable_all_tracing(Connected, Node, Cookie) when Connected=:=true ->
    ok = disable_tracing(Node, []),
    case rpc:call(Node, dbg, stop_clear, []) of
        ok ->
            ok;
        {badrpc,nodedown} ->
            ok
    end,
    %% Just make sure dbg, and tracer is always started..
    case dbg_start(Node) of
        {ok, _RemoteDbgPid} ->
            [{Node, Cookie, Type}] = goanna_db:lookup([nodelist, Node]),
            true = goanna_db:delete_child_id_tracelist(Node, Cookie),
            trace_steps(Node, Cookie, Type);
        {badrpc,nodedown} ->
            {ok, undefined}
    end.
%%------------------------------------------------------------------------
disable_tracing(Node, []) ->
    ?DEBUG("[~p] [~p] Disable all tracing", [?MODULE, Node]),
    case rpc:call(Node, dbg, ctpl, []) of
        {badrpc, nodedown} ->
            ok;
        {ok, MatchDesc} ->
            ?DEBUG("[~p] [~p] dbg:ctpl MatchDesc ~p",
                [?MODULE, Node, MatchDesc])
    end;
disable_tracing(Node, T=#trc_pattern{m=Module,f=undefined,a=undefined}) ->
    ?DEBUG("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    case rpc:call(Node, dbg, ctpl, [Module]) of
        {badrpc, nodedown} ->
            ok;
        {ok, MatchDesc} ->
            ?DEBUG("[~p] [~p] dbg:ctpl MatchDesc ~p",
                [?MODULE, Node, MatchDesc])
    end;
disable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=undefined}) ->
    ?DEBUG("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    case rpc:call(Node, dbg, ctpl, [Module, Function]) of
        {badrpc, nodedown} ->
            ok;
        {ok, MatchDesc} ->
            ?DEBUG("[~p] [~p] dbg:ctpl MatchDesc ~p",
                [?MODULE, Node, MatchDesc])
    end;
disable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=Arity}) ->
    ?DEBUG("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    case rpc:call(Node, dbg, ctpl, [Module, Function, Arity]) of
        {badrpc, nodedown} ->
            ok;
        {ok, MatchDesc} ->
            ?DEBUG("[~p] [~p] dbg:ctpl MatchDesc ~p",
                [?MODULE, Node, MatchDesc])
    end.
%%------------------------------------------------------------------------
-spec handle_data_retrival_method({push, non_neg_integer(), atom()} | pull) -> reference() | undefined.
handle_data_retrival_method({push, Interval, Mod}) ->
    PushTRef = erlang:send_after(Interval, self(), {push_data, Mod}),
    _PendingPush=PushTRef;
handle_data_retrival_method(pull) ->
    _PendingPush=undefined.

push_data_loop(Mod, Tbl, Node) ->
    First =
        try
            ets:first(Tbl)
        catch
            C:E ->
                ?ALERT("ets:first got exception ~p ~p", [C, E]),
                '$end_of_table'
        end,
    push_data_loop(Mod, Tbl, Node, First).

%% TODO: Simplify, and abstract out, this simple db table walk
push_data_loop(_Mod, _Tbl, _Node, '$end_of_table') ->
    ok;
push_data_loop(Mod, Tbl, Node, Key) ->
    [E] = goanna_db:lookup([trace, Tbl, Key]),
    ok = Mod:forward(Tbl, E),
    true = ets:delete(Tbl, Key),
    push_data_loop(Mod, Tbl, Node, ets:next(Tbl, Key)).
%%------------------------------------------------------------------------
new_trace_timer(_ChildId, false, false) ->
    undefined;
new_trace_timer(_ChildId, false, TRef) ->
    ok = cancel_timer(TRef),
    undefined;
new_trace_timer(ChildId, NewTraceTime, false) ->
    ?WARNING("STOPPING traces after ~p Seconds", [NewTraceTime/1000]),
    {ok, TRef} = timer:apply_after(NewTraceTime, gen_server, call, [ChildId, stop_all_trace_patterns]),
    TRef;
new_trace_timer(ChildId, NewTraceTime, {_, TRef}) when is_reference(TRef) ->
    ok = cancel_timer(TRef),
    ?WARNING("STOPPING traces after ~p Seconds", [NewTraceTime/1000]),
    {ok, NewTRef} = timer:apply_after(NewTraceTime, gen_server, call, [ChildId, stop_all_trace_patterns]),
    NewTRef.

cancel_timer(TRef) ->
    try
        case timer:cancel(TRef) of
            {error,badarg} ->
                ok;
            {ok, cancel} ->
                ok
        end
    catch
        C:E ->
            ?EMERGENCY("Could not clear timer: ~p ~p~n~p", [C,E,erlang:get_stacktrace()]),
            ok
    end.
%%------------------------------------------------------------------------
-spec list_property_or_default(term(), proplists:proplist(), term()) -> term().
list_property_or_default(Field, Opts, Default) ->
    case lists:keyfind(Field, 1, Opts) of
        false          -> Default;
        {Field, Value} -> Value
    end.

%% TODO: Fix re-apply traces... i changed the tracelist
-spec reapply_traces(#?STATE{}) -> #?STATE{}.
reapply_traces(#?STATE{node = Node,
                       child_id=ChildId,
                       trace_time=TraceTime} = State) ->
    TraceReapplyRes = [
        begin
            %% TODO: check if the trace time is still valid: ! Created Timestamp
            % {trc, TrcPattern} = lists:keyfind(trc, 1, Opts),
            ?CRITICAL("!!! re-tracing - ~p", [TrcPattern]),
            ok = enable_tracing(Node, TrcPattern)
        end
    || {{C, TrcPattern},_Timestamp, _Opts} <- ets:tab2list(tracelist), C =:=ChildId],
    case TraceReapplyRes of
        [] ->
            State;
        _Else ->
            State#?STATE{ % Take trace_msg_total & trace_time, from what was set at init()
                trace_timer_tref = new_trace_timer(ChildId, TraceTime, false), % There couldn't have been a timer...
                trace_active = true
            }
    end.

-spec check_forward_mod(undefined | atom()) -> ok.
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
            ?EMERGENCY("[~p] Forwarding callback module ~p
                missing required behaviour functions...halting...",
                [?MODULE, Mod]),
            erlang:halt(1),
            ok
    end.