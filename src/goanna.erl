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
                 poll_forward_callback_mod,
                 trace_msg_count=0,
                 trace_msg_total=100, %% Any callback module...
                 trace_pattern_timer_ref=[],
                 remote_trace_enabled=false
                }).
%%------------------------------------------------------------------------
start_link(_NodeObj = {Node, Cookie, Type}) ->
    Name = goanna_sup:id(Node, Cookie),
    gen_server:start_link({local, Name},
                          ?MODULE, {Node,Cookie,Type}, []).

init({Node, Cookie, Type}) ->
	Mod = application:get_env(goanna, poll_forward_callback_mod, goanna_shell_printer),
    case Mod of
        goanna_shell_printer ->
	       c:l(goanna_shell_printer);
        _ ->
            ok
    end,
	case erlang:function_exported(Mod, forward, 2) of
		true ->
            est_rem_conn(#?STATE{node=Node,
                                 cookie=Cookie,
                                 type=Type,
                                 poll_forward_callback_mod=Mod
            });
        false ->
            erlang:halt(1)
    end.
%%------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------
%% Connectivity-----------------------------------------------------------
handle_info({nodedown, Node}, #?STATE{ node=Node } = State) ->
    ?INFO("[~p] Node:~p down...", [?MODULE, Node]),
    {noreply, reconnect(State)};
handle_info(reconnect, #?STATE{ node = Node } = State) ->
    ?INFO("[~p] attempting to reconnect to ~p...", [?MODULE, Node]),
    {ok, NewState} = est_rem_conn(State),
    {noreply, NewState};
%%------------------------------------------------------------------------
%%---Disable Tracing------------------------------------------------------
handle_info(stop_trace, #?STATE{node=Node, cookie=Cookie} = State) ->
    disable_all_tracing(Node, Cookie),
    {noreply, State#?STATE{trace_msg_count = 0,
                           trace_pattern_timer_ref = [],
                           remote_trace_enabled = false}};
handle_info({stop_trace, TrcPattern}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    goanna_db:delete_tracelist_pattern([Node, Cookie], TrcPattern),
    disable_tracing(Node, TrcPattern),
    {noreply, State#?STATE{ trace_pattern_timer_ref = lists:keydelete(TrcPattern, 1, State#?STATE.trace_pattern_timer_ref) }};
%%------------------------------------------------------------------------
%%---Tracing--------------------------------------------------------------
handle_info({trace, Opts}, #?STATE{ node = Node } = State) ->
    %% TODO: make sure a MODULE is loaded on the remote node... ?
    {trc, TrcPattern} = lists:keyfind(trc, 1, Opts),
    % case goanna_db:store([trace_pattern, Node, Cookie], TrcPattern) of
    case lists:keyfind(TrcPattern, 1, State#?STATE.trace_pattern_timer_ref) of
        false ->
            enable_tracing(Node, TrcPattern, State#?STATE.remote_trace_enabled),
            {time, MSecTime} = lists:keyfind(time, 1, Opts),
            TRef = erlang:send_after(MSecTime, self(), {stop_trace, TrcPattern}),
            {noreply, State#?STATE{ trace_pattern_timer_ref = [{TrcPattern, TRef}|State#?STATE.trace_pattern_timer_ref] }};
        _ ->
            {noreply, State}
    end;
%%------------------------------------------------------------------------
%%---Poll results, and forward upwards------------------------------------
handle_info({poll_results}, #?STATE{ node = Node,
									 cookie = Cookie,
									 poll_forward_callback_mod = Mod } = State) ->
	ok = poll_table(Mod, Node, Cookie),
    polling(),
	{noreply, State};
%%------------------------------------------------------------------------
%%---Deal with message counts---------------------------------------------
%% TODO: Should we have counts per trace pattern, or a count for everything ?
handle_info({trace_item}, #?STATE{ node=_Node, cookie=_Cookie,
                                   trace_msg_count=TMC, trace_msg_total=TMT } = State) when TMC<TMT ->
    {noreply, State#?STATE{trace_msg_count = State#?STATE.trace_msg_count + 1}};
handle_info({trace_item}, #?STATE{ node=Node, cookie=Cookie,
                                   trace_msg_count=TMC, trace_msg_total=TMT } = State) when TMC>=TMT ->
    ?INFO("[~p] [~p] Trace message count limit reached...", [?MODULE, Node]),
    disable_all_tracing(Node, Cookie),
    {noreply, State#?STATE{trace_msg_count = 0,
                           trace_pattern_timer_ref = [],
                           remote_trace_enabled = false}};
%%------------------------------------------------------------------------
%%---Shoudn't happen, but hey... let's see ...----------------------------
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?INFO("Info ~p : ~p", [goanna_sup:id(Node, Cookie), Info]),
    {noreply, State}.

terminate(Reason, #?STATE{ node=Node } = _State) ->
    ?INFO("[~p] terminate ~p ", [?MODULE, Reason]),
    true = goanna_db:delete_node(Node),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%------------------------------------------------------------------------

est_rem_conn(#?STATE{ node=Node, cookie=Cookie, type=Type } = State) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            trace_steps(Node, Cookie, Type),
            do_monitor_node(Node, State#?STATE.connect_attempt_ref),
            polling(),
            {ok, State#?STATE{connected=true,
                               connect_attempt_ref = undefined,
                               connect_attemps = 0,
                               remote_trace_enabled = true }};
        false ->
            {ok, reconnect(State)}
    end.

do_rem_conn(Node, Cookie) ->
    ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))).

do_monitor_node(Node, ConnectAttemptTref) ->
    true = erlang:monitor_node(Node, true),
    case ConnectAttemptTref of
        undefined -> ok;
        TRef      -> timer:cancel(TRef)
    end,
    ?INFO("[~p] ----------------- NODE ~p STARTUP COMPLETED -----------------\n\n",
        [?MODULE, Node]).

trace_steps(Node, Cookie, tcpip_port) ->
    tcpip_port_trace_steps(Node, Cookie),
    reapply_traces(Node, Cookie);
trace_steps(_Node, _Cookie, file) ->
    ?WARNING("dbg trace_port file - fearure not implemented..."),
    erlang:halt(1);
trace_steps(Node, Cookie, erlang_distribution) ->
    erlang_distribution_trace_steps(Node, Cookie),
    reapply_traces(Node, Cookie).

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
                {{case_clause,DbgPid},[{dbg,start,1,_},_]}}}} ->
            {ok, DbgPid};
        C:E ->
            ?WARNING("[~p] dbg start failed ~p", [?MODULE, {C,E}])
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

%% TODO: how do i deal with, Trace counts and trace time, when re-applying?
%% i would have to implement something remotely that stops maybe...
reapply_traces(Node, Cookie) ->
    case goanna_db:lookup([trc_pattern, Node, Cookie]) of
        [] ->
            ok;
        TracePatternRecs ->
            FRec = fun({_Key, TrcPatternList}) ->
                ok = lists:foreach(fun(TrcPattern) ->
                    enable_tracing(Node, TrcPattern, true)
                end, TrcPatternList)
            end,
            lists:map(FRec, lists:flatten(TracePatternRecs))
    end.

reconnect(#?STATE{ connect_attemps = Attempts } = State) when Attempts > 10 ->
    ?CRITICAL("1000"),
    TRef = erlang:send_after(1000, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef,
                 connect_attemps = Attempts+1
    };
reconnect(#?STATE{ connect_attemps = Attempts } = State) when Attempts > 3 ->
    ?CRITICAL("250"),
    TRef = erlang:send_after(250, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef,
                 connect_attemps = Attempts+1
    };
reconnect(#?STATE{ connect_attemps = Attempts } = State) when Attempts =< 3 ->
    ?CRITICAL("50"),
    TRef = erlang:send_after(50, self(), reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef,
                 connect_attemps = Attempts+1
    }.

enable_tracing(Node, TrcPattern, false) ->
    [{Node, Cookie, Type}] = goanna_db:lookup([nodelist, Node]),
    trace_steps(Node, Cookie, Type),
    enable_tracing(Node, TrcPattern, true);
enable_tracing(_Node, false, _) ->
    {error, nothing_to_trace};
enable_tracing(Node, T=#trc_pattern{m=Module,f=undefined,a=undefined}, true) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

enable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=undefined}, true) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

enable_tracing(Node, T=#trc_pattern{m=Module,f=Function,a=Arity}, true) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl,[Module, Function, Arity]),
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

poll_table(Mod, Node, Cookie) ->
	Tbl=goanna_sup:id(Node, Cookie),
	trace_results_loop(Mod, Tbl).

polling() ->
    Interval = application:get_env(goanna, poll_interval, 1000),
    case application:get_env(goanna, poll_data, false) of
        true ->
            erlang:send_after(Interval, self(), {poll_results});
        false ->
            ok
    end.

trace_results_loop(Mod, Tbl) ->
    trace_results_loop(Mod, Tbl, ets:first(Tbl)).

%% TODO: Simplify, and abstract out, this simple db table walk
trace_results_loop(_Mod, _Tbl, '$end_of_table') ->
    ok;
trace_results_loop(Mod, Tbl, Key) ->
    ok = Mod:forward(Tbl, goanna_db:lookup([trace, Tbl, Key])),
    true = ets:delete(Tbl, Key),
    trace_results_loop(Mod, Tbl, ets:next(Tbl, Key)).

disable_all_tracing(Node, Cookie) ->
    ok = rpc:call(Node, dbg, stop_clear, []),
    true = goanna_db:truncate_tracelist([Node, Cookie]),

    %% Just make sure dbg is always started..
    {ok, _RemoteDbgPid} = dbg_start(Node),
    disable_tracing(Node, []).