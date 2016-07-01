-module (goanna).

-behaviour(gen_server).

-export([
    start/0,
    trace/1, trace/2, trace/3,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3
]).

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
                 node,
                 cookie,
                 type,
                 tracing=false %% Build a check, when enabling/disabling
                }).
%%------------------------------------------------------------------------
%% API
start() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(compiler),
    ok = application:start(inets),
    ok = application:start(syntax_tools),
    ok = application:start(sasl),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(goanna).

trace(Module) ->
    cluster_foreach({trace, Module}).

trace(Module, Function) ->
    cluster_foreach({trace, Module, Function}).

trace(Module, Function, Arity) ->
    cluster_foreach({trace, Module, Function, Arity}).

stop_trace() ->
    cluster_foreach(stop_trace).

stop_trace(Module) ->
    cluster_foreach({stop_trace, Module}).

stop_trace(Module, Function) ->
    cluster_foreach({stop_trace, Module, Function}).

stop_trace(Module, Function, Arity) ->
    cluster_foreach({stop_trace, Module, Function, Arity}).

cluster_foreach(Msg) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            whereis(goanna_sup:id(Node, Cookie)) ! Msg
        end, ets:tab2list(nodelist)
    ).
%%------------------------------------------------------------------------
start_link(_NodeObj={Node,Cookie,Type}) ->
    Name = goanna_sup:id(Node, Cookie),
    gen_server:start_link({local, Name},
                          ?MODULE, {Node,Cookie,Type}, []).

init({Node,Cookie,Type}) ->
    est_rem_conn(#?STATE{node=Node,
                         cookie=Cookie,
                         type=Type}).

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
    ok = rpc:call(Node, dbg, stop_clear, []),
    true = goanna_db:truncate_tracelist([Node, Cookie]),
    disable_tracing(Node, []),
    {noreply, State#?STATE{ tracing = false }};
handle_info({stop_trace, Module}, #?STATE{ node = Node } = State) ->
    disable_tracing(Node, [Module]),
    {noreply, State#?STATE{ tracing = false }};
handle_info({stop_trace, Module, Function}, #?STATE{ node = Node } = State) ->
    disable_tracing(Node, [Module, Function]),
    {noreply, State#?STATE{ tracing = false }};
handle_info({stop_trace, Module, Function, Arity}, #?STATE{ node = Node } = State) ->
    disable_tracing(Node, [Module, Function, Arity]),
    {noreply, State#?STATE{ tracing = false }};
%%------------------------------------------------------------------------
%%---Tracing--------------------------------------------------------------
handle_info({trace, Module}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    case
        goanna_db:store([trace_pattern, Node, Cookie],
            #trc_pattern{m=Module})
    of
        true ->
            enable_tracing(Node, [Module]);
        {error, already_traced} ->
            ok
    end,
    {noreply, State#?STATE{ tracing = true }};
handle_info({trace, Module, Function}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    case
        goanna_db:store([trace_pattern, Node, Cookie],
            #trc_pattern{m=Module,f=Function})
    of
        true ->
            enable_tracing(Node, [Module, Function]);
        {error, already_traced} ->
            ok
    end,
    {noreply, State#?STATE{ tracing = true }};
handle_info({trace, Module, Function, Arity}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    case
        goanna_db:store([trace_pattern, Node, Cookie],
            #trc_pattern{m=Module,f=Function,a=Arity})
    of
        true ->
            enable_tracing(Node, [Module, Function, Arity]);
        {error, already_traced} ->
            ok
    end,
    {noreply, State#?STATE{ tracing = true }};
%%------------------------------------------------------------------------
%%---Shoudn't happen, but hey... let's see ...----------------------------
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?INFO("Info ~p : ~p", [goanna_sup:id(Node, Cookie), Info]),
    {noreply, State}.

terminate(Reason, #?STATE{ node=Node, cookie=Cookie } = _State) ->
    ?INFO("[~p] terminate ~p ", [?MODULE, Reason]),
    true = goanna_db:terminate_node([Node, Cookie]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%------------------------------------------------------------------------

est_rem_conn(#?STATE{ node=Node, cookie=Cookie } = State) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            State2 = trace_steps(State),
            do_monitor_node(Node, State2#?STATE.connect_attempt_ref),
            {ok, State2#?STATE{connected=true, connect_attempt_ref = undefined }};
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
        goanna_db:store([trace, Node, Cookie],Trace)
    end};
handler_fun(_Node, _Cookie, file) ->
    {ok, undefined};
handler_fun(Node, Cookie, erlang_distribution) ->
    FunStr = lists:flatten(io_lib:format(
        "fun(Trace, _) ->"
        " true=rpc:call(~p, goanna_db, store, [[trace,~p,~p],Trace]) "
        "end.", [node(), Node, Cookie])),
    {ok, Tokens, _} = erl_scan:string(FunStr),
    {ok,[Form]} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:add_binding('B', 2, erl_eval:new_bindings()),
    {value, Fun, _} = erl_eval:expr(Form, Bindings),
    {ok, Fun}.

reapply_traces(#?STATE{node = Node, cookie = Cookie} = State) ->
    case goanna_db:lookup([trc_pattern, Node, Cookie]) of
        [] ->
            State;
        TracePatternRecs ->
            FRec = fun({_Key, TrcPatternList}) -> TrcPatternList end,
            TracePatterns = lists:flatten(lists:map(FRec, TracePatternRecs)),
            F =
            fun (#trc_pattern{m=Module,f=undefined,a=undefined}) ->
                    enable_tracing(Node, [Module]);
                (#trc_pattern{m=Module,f=Func,a=undefined}) ->
                    enable_tracing(Node, [Module, Func]);
                (#trc_pattern{m=Module,f=Func,a=Arity}) ->
                    enable_tracing(Node, [Module, Func, Arity])
            end,
            ok = lists:foreach(F, TracePatterns),
            State#?STATE{ tracing = true }
    end.

reconnect(State) ->
    {ok, TRef} = timer:send_after(250, reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef}.

enable_tracing(Node, T=[Module]) ->
    ?INFO("[~p] enable_tracing:~p~n", [?MODULE, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    ?INFO("dbg:tpl MatchDesc ~p", [MatchDesc]);

enable_tracing(Node, T=[Module, Function]) ->
    ?INFO("[~p] enable_tracing:~p~n", [?MODULE, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, cx]),
    ?INFO("dbg:tpl MatchDesc ~p", [MatchDesc]);

enable_tracing(Node, T=[Module, Function, Arity]) ->
    ?INFO("[~p] enable_tracing:~p~n", [?MODULE, T]),
    {ok, MatchDesc} = rpc:call(Node, dbg, tpl,[Module, Function, Arity]),
    ?INFO("dbg:tpl MatchDesc ~p", [MatchDesc]).

disable_tracing(Node, []) ->
    ?INFO("[~p] Disable all tracing", [?MODULE]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, []),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]);

disable_tracing(Node, T=[Module]) ->
    ?INFO("[~p] Disable ~p tracing", [?MODULE, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module]),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]);

disable_tracing(Node, T=[Module, Function]) ->
    ?INFO("[~p] Disable ~p tracing", [?MODULE, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module, Function]),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]);

disable_tracing(Node, T=[Module, Function, Arity]) ->
    ?INFO("[~p] Disable ~p tracing", [?MODULE, T]),
    {ok,MatchDesc} = rpc:call(Node, dbg, ctpl, [Module, Function, Arity]),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]).