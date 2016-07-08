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
                 trace_forward_callback,
                 trace_msg_count=0,
                 trace_msg_total=100 %% Any callback module...
                }).
%%------------------------------------------------------------------------
start_link(_NodeObj = {Node, Cookie, Type}) ->
    Name = goanna_sup:id(Node, Cookie),
    gen_server:start_link({local, Name},
                          ?MODULE, {Node,Cookie,Type}, []).

init({Node, Cookie, Type}) ->
	Mod = application:get_env(goanna, trace_forward_callback, goanna_shell_printer),
	c:l(goanna_shell_printer),
	case erlang:function_exported(Mod, forward, 2) of
		true ->
            est_rem_conn(#?STATE{node=Node,
                                 cookie=Cookie,
                                 type=Type,
                                 trace_forward_callback=Mod
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
    {noreply, State#?STATE{trace_msg_count = 0}};
handle_info({stop_trace, Opts}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    TrcPattern = lists:keyfind(trc, 1, Opts),
    goanna_db:delete_tracelist_pattern([Node, Cookie], TrcPattern),
    disable_tracing(Node, TrcPattern),
    {noreply, State};
%%------------------------------------------------------------------------
%%---Tracing--------------------------------------------------------------
handle_info({trace, Opts}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    TrcPattern = lists:keyfind(trc, 1, Opts),
    case goanna_db:store([trace_pattern, Node, Cookie], TrcPattern) of
        true ->
            enable_tracing(Node, TrcPattern),
            {time, MSecTime} = lists:keyfind(time, 1, Opts),
            TRef = erlang:send_after(MSecTime, self(), stop_trace),
            {noreply, State};
        {error, already_traced} ->
            {noreply, State}
    end;
%%------------------------------------------------------------------------
%%---Poll results, and forward upwards------------------------------------
handle_info({poll_results}, #?STATE{ node = Node,
									 cookie = Cookie,
									 trace_forward_callback = Mod } = State) ->
	ok = poll_table(Mod, Node, Cookie),
	erlang:send_after(application:get_env(goanna, poll_interval, 1000), self(), {poll_results}),
	{noreply, State};
%%------------------------------------------------------------------------
%%---Deal with message counts---------------------------------------------
handle_info({trace_item}, #?STATE{ node=_Node, cookie=_Cookie,
                                   trace_msg_count=TMC, trace_msg_total=TMT } = State) when TMC<TMT ->
    {noreply, State#?STATE{trace_msg_count = State#?STATE.trace_msg_count + 1}};
handle_info({trace_item}, #?STATE{ node=Node, cookie=Cookie,
                                   trace_msg_count=TMC, trace_msg_total=TMT } = State) when TMC>=TMT ->
    ?INFO("[~p] [~p] Trace message count limit reached...", [?MODULE, Node]),
    disable_all_tracing(Node, Cookie),
    {noreply, State#?STATE{trace_msg_count = 0}};
%%------------------------------------------------------------------------
%%---Shoudn't happen, but hey... let's see ...----------------------------
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?INFO("Info ~p : ~p", [goanna_sup:id(Node, Cookie), Info]),
    {noreply, State}.

terminate(Reason, #?STATE{ node=Node, cookie=Cookie } = _State) ->
    ?INFO("[~p] terminate ~p ", [?MODULE, Reason]),
    true = goanna_db:delete_node(Node),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%------------------------------------------------------------------------

est_rem_conn(#?STATE{ node=Node, cookie=Cookie } = State) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            State2 = trace_steps(State),
            do_monitor_node(Node, State2#?STATE.connect_attempt_ref),
            erlang:send_after(1, self(), {poll_results}),
            {ok, State2#?STATE{connected=true,
                               connect_attempt_ref = undefined,
                               connect_attemps = 0 }};
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

trace_steps(#?STATE{node = Node,
                    cookie = Cookie,
                    type = tcpip_port} = State) ->
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
    ?INFO("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
    reapply_traces(State);
trace_steps(#?STATE{node = _Node,
                    cookie = _Cookie,
                    type = file} = State) ->
    ?WARNING("dbg trace_port file - fearure not implemented..."),
    erlang:halt(1),
    reapply_traces(State);
trace_steps(#?STATE{node = Node,
                    cookie = Cookie,
                    type = erlang_distribution} = State) ->
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
    ?INFO("[~p] Node:~p MatchDesc:~p", [?MODULE, Node, MatchDesc]),
    reapply_traces(State).

dbg_start(Node) ->
    try
        {ok, Pid} = rpc:call(Node, dbg, start, [])
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

reapply_traces(#?STATE{node = Node, cookie = Cookie} = State) ->
    case goanna_db:lookup([trc_pattern, Node, Cookie]) of
        [] ->
            ok;
        TracePatternRecs ->
            FRec = fun({_Key, TrcPatternList}) ->
                ok = lists:foreach(fun(TrcPattern) ->
                    enable_tracing(Node, TrcPattern)
                end, TrcPatternList)
            end,
            lists:map(FRec, lists:flatten(TracePatternRecs))
    end,
    State.

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

enable_tracing(_Node, false) ->
    {error, nothing_to_trace};
enable_tracing(Node, {trc, T=#trc_pattern{m=Module,f=undefined,a=undefined}}) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

enable_tracing(Node, {trc, T=#trc_pattern{m=Module,f=Function,a=undefined}}) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, cx]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

enable_tracing(Node, {trc, T=#trc_pattern{m=Module,f=Function,a=Arity}}) ->
    ?INFO("[~p] [~p] enable_tracing:~p~n", [?MODULE, Node, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl,[Module, Function, Arity]),
    ?INFO("[~p] [~p] dbg:tpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]).

disable_tracing(Node, []) ->
    ?INFO("[~p] [~p] Disable all tracing", [?MODULE, Node]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, []),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

disable_tracing(Node, {trc, T=#trc_pattern{m=Module,f=undefined,a=undefined}}) ->
    ?INFO("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module]),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

disable_tracing(Node, {trc, T=#trc_pattern{m=Module,f=Function,a=undefined}}) ->
    ?INFO("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module, Function]),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]);

disable_tracing(Node, {trc, T=#trc_pattern{m=Module,f=Function,a=Arity}}) ->
    ?INFO("[~p] [~p] Disable ~p tracing", [?MODULE, Node, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module, Function, Arity]),
    ?INFO("[~p] [~p] dbg:ctpl MatchDesc ~p",
        [?MODULE, Node, MatchDesc]).

poll_table(Mod, Node, Cookie) ->
	Tbl=goanna_sup:id(Node, Cookie),
	trace_results_loop(Mod, Tbl).

trace_results_loop(Mod, Tbl) ->
    trace_results_loop(Mod, Tbl, ets:first(Tbl)).

trace_results_loop(_Mod, _Tbl, '$end_of_table') ->
    ok;
trace_results_loop(Mod, Tbl, Key) ->
	ok = Mod:forward(Tbl, ets:lookup(Tbl, Key)),
    true = ets:delete(Tbl, Key),
    trace_results_loop(Mod, Tbl, ets:next(Tbl, Key)).

disable_all_tracing(Node, Cookie) ->
    ok = rpc:call(Node, dbg, stop_clear, []),
    true = goanna_db:truncate_tracelist([Node, Cookie]),
    disable_tracing(Node, []).