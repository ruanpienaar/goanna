-module(goanna).

-behaviour(gen_server).
-export([
    start/0,
    trace/1, trace/2, trace/3,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3
]).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-define(STATE, goanna_state).
-record(?STATE, {connected=false,
                 connect_attempt_ref=undefined,
                 node,
                 cookie,
                 matchdesc,
                 tracing=false,
                 trace_module
                }).
-include_lib("goanna.hrl").

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
        fun({Node, Cookie}) ->
            whereis(goanna_sup:id(Node, Cookie)) ! Msg
        end, ets:tab2list(nodelist)
    ).
%%------------------------------------------------------------------------
start_link(Node, Cookie) ->
    Name = goanna_sup:id(Node, Cookie),
    gen_server:start_link({local, Name},
                          ?MODULE, {Node, Cookie}, []).

init({Node, Cookie}) ->
    est_rem_conn(#?STATE{node=Node, cookie=Cookie}, startup).
%%------------------------------------------------------------------------
est_rem_conn(#?STATE{ node=Node, cookie=Cookie } = State, Action) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            ?INFO("[~p] Node:~p Cookie:~p Connected", [?MODULE, Node, Cookie]),
            case Action of
                reconnect ->
                    ?INFO("[~p] Attempting to re-add the other nodes", [?MODULE]),
                    add_nodes(Node, Cookie);
                _ ->
                    ok
            end,
            State2 = trace_steps(State),
            {ok, State2#?STATE{connected=true, connect_attempt_ref = undefined }};
        false ->
            {ok, reconnect(State)}
    end.

do_rem_conn(Node, Cookie) ->
    ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))).
%%------------------------------------------------------------------------
trace_steps(#?STATE{node = Node, cookie = Cookie} = State) ->
    State2 = do_tracing(State, Node, Cookie),
    ?INFO("[~p] dbg process dictionary: ~p~n",
        [?MODULE, erlang:process_info(whereis(dbg), [dictionary])]),
    true = erlang:monitor_node(Node, true),
    case State2#?STATE.connect_attempt_ref of
        undefined -> ok;
        TRef      -> timer:cancel(TRef)
    end,
    ?INFO("[~p] ----------------- NODE ~p STARTUP COMPLETED -----------------\n\n",
        [?MODULE, Node]),
    State2.

do_tracing(State, Node, Cookie) ->
    case node() of
        Node ->
            %% Don't apply any tracing on the local node...
            State;
        _ ->
            add_node(Node),
            {ok, MatchDesc} = dbg:p(all, call),
            ?INFO("[~p] dbg:p(all, call) -> ~p", [?MODULE, MatchDesc]),
            % ?INFO("dbg nodes:"),
            % dbg:ln(),

            %% Re-enable tracing on remote node:
            case goanna_db:lookup([trc_pattern, Node, Cookie]) of
                [] ->
                    State;
                TracePatternRecs ->
                    FRec = fun({_Key, TrcPatternList}) -> TrcPatternList end,
                    TracePatterns = lists:flatten(lists:map(FRec, TracePatternRecs)),
                    F =
                    fun (#trc_pattern{m=Module,f=undefined,a=undefined}) ->
                            enable_tracing([Module]);
                        (#trc_pattern{m=Module,f=Func,a=undefined}) ->
                            enable_tracing([Module, Func]);
                        (#trc_pattern{m=Module,f=Func,a=Arity}) ->
                            enable_tracing([Module, Func, Arity])
                    end,
                    ok = lists:foreach(F, TracePatterns),
                    State#?STATE{ tracing = true }
            end
    end.
%%------------------------------------------------------------------------
add_node(Node) ->
    case dbg:n(Node) of
        {ok, Node} ->
            ?INFO("[~p] dbg:n -> {ok,~p}", [?MODULE, Node]),
            ok;
        {error,{already_started,Node}} ->
            ?INFO("[~p] dbg:n -> {error,{already_started,~p}}", [?MODULE, Node]);
        {error,{nodedown,Node}} ->
            ?INFO("[~p] dbg:n -> {error,{nodedown,~p}}", [?MODULE, Node]);
        E ->
            ?INFO("[~p] dbg:n -> ~p", [?MODULE, E])
    end.
add_nodes() ->
    lists:foreach(
        fun
        ({OtherNode, _C}) ->
            ?INFO("[~p] READDING ~p dbg:n/1", [?MODULE, OtherNode]),
            add_node(OtherNode)
        end, ets:tab2list(nodelist)
    ).
add_nodes(ExcludeNode, ExcludeCookie) ->
    lists:foreach(
        fun
        ({N, C}) when N==ExcludeNode, C==ExcludeCookie ->
            ok;
        ({OtherNode, _C}) ->
            ?INFO("[~p] READDING ~p dbg:n/1", [?MODULE, OtherNode]),
            add_node(OtherNode)
        end, ets:tab2list(nodelist)
    ).
%%------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, #?STATE{ node=Node } = State) ->
    ?INFO("[~p] Node:~p down...", [?MODULE, Node]),
    {noreply, reconnect(State)};
handle_info(reconnect, #?STATE{ node = _Node } = State) ->
    ?INFO("[~p] attempting to reconnect...", [?MODULE]),
    {ok, NewState} = est_rem_conn(State, reconnect),
    {noreply, NewState};
%% --- Disable Tracing
handle_info(stop_trace, #?STATE{node=Node, cookie=Cookie} = State) ->
    ok = dbg:stop_clear(),
    ok = goanna_db:truncate_tracelist([Node, Cookie]),
    %disable_tracing([]),
    % {noreply, State#?STATE{ tracing = false }};
    {stop, stop_tracing, State};
handle_info({stop_trace, Module}, State) ->
    disable_tracing([Module]),
    {noreply, State#?STATE{ tracing = false }};
    % {stop, stop_tracing, State};
handle_info({stop_trace, Module, Function}, State) ->
    disable_tracing([Module, Function]),
    {noreply, State#?STATE{ tracing = false }};
    % {stop, stop_tracing, State};
handle_info({stop_trace, Module, Function, Arity}, State) ->
    disable_tracing([Module, Function, Arity]),
    {noreply, State#?STATE{ tracing = false }};
    % {stop, stop_tracing, State};
%% --- Tracing
handle_info({trace, Module}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    case
        goanna_db:store([trace_pattern, Node, Cookie],
            #trc_pattern{m=Module})
    of
        true ->
            enable_tracing([Module]);
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
            enable_tracing([Module, Function]);
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
            enable_tracing([Module, Function, Arity]);
        {error, already_traced} ->
            ok
    end,
    {noreply, State#?STATE{ tracing = true }};
%% --- Shoudn't happen, but hey... let's see ...
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?INFO("Info ~p : ~p", [goanna_sup:id(Node, Cookie), Info]),
    {noreply, State}.
%%------------------------------------------------------------------------
enable_tracing(T=[Module]) ->
    ?INFO("[~p] enable_tracing:~p~n", [?MODULE, T]),
    {ok, MatchDesc} = dbg:tpl(Module, cx),
    ?INFO("dbg:tpl MatchDesc ~p", [MatchDesc]);
enable_tracing(T=[Module, Function]) ->
    ?INFO("[~p] enable_tracing:~p~n", [?MODULE, T]),
    {ok, MatchDesc} = dbg:tpl(Module, Function, cx),
    ?INFO("dbg:tpl MatchDesc ~p", [MatchDesc]);
enable_tracing(T=[Module, Function, Arity]) ->
    ?INFO("[~p] enable_tracing:~p~n", [?MODULE, T]),
    {ok, MatchDesc} = dbg:tpl(Module, Function, Arity, cx),
    ?INFO("dbg:tpl MatchDesc ~p", [MatchDesc]).

disable_tracing([]) ->
    ?INFO("[~p] Disable all tracing", [?MODULE]),
    {ok, MatchDesc} = dbg:ctpl(),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]);
disable_tracing(T=[Module]) ->
    ?INFO("[~p] Disable ~p tracing", [?MODULE, T]),
    {ok, MatchDesc} = dbg:ctpl(Module),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]);
disable_tracing(T=[Module, Function]) ->
    ?INFO("[~p] Disable ~p tracing", [?MODULE, T]),
    {ok, MatchDesc} = dbg:ctpl(Module, Function),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]);
disable_tracing(T=[Module, Function, Arity]) ->
    ?INFO("[~p] Disable ~p tracing", [?MODULE, T]),
    {ok, MatchDesc} = dbg:ctpl(Module, Function, Arity),
    ?INFO("dbg:ctpl MatchDesc ~p", [MatchDesc]).
%%------------------------------------------------------------------------
terminate(Reason, #?STATE{node = Node, cookie = Cookie} = _State) ->
    ?INFO("[~p] terminate ~p ", [?MODULE, Reason]),
    true = goanna_db:terminate_node([Node, Cookie]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%------------------------------------------------------------------------
reconnect(State) ->
    {ok, TRef} = timer:send_after(250, reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef}.