-module(goanna).

-behaviour(gen_server).
-export([
    start/0,
    trace/1, trace/2, trace/3,
    stop_trace/0
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
    ?INFO("STARTING ................... \n"),
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
    goanna_db:init_node([Node, Cookie]),
    est_rem_conn(#?STATE{node=Node, cookie=Cookie}, startup).

est_rem_conn(#?STATE{ node=Node, cookie=Cookie } = State, Action) ->
    case do_rem_conn(Node, Cookie) of
        true ->
            ?INFO("Connected..."),

            case Action of
                reconnect ->
                    ?INFO("Attempting to re-add the other nodes"),
                    % goanna_tracer:tracer(),
                    lists:foreach(
                        fun
                        ({N, C}) when N==Node, C==Cookie ->
                            ok;
                        ({OtherNode, _C}) ->
                            ?INFO("RE-ADDING ~p dbg:n/1", [Node]),
                            add_node(OtherNode)
                        end, ets:tab2list(nodelist)
                    );
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

trace_steps(#?STATE{node = Node, cookie = Cookie} = State) ->
    ?INFO("------------------"),
    ?INFO("TRACER PID: ~p", [whereis(dbg)]),
    ?INFO("~p~n", [ erlang:process_info(whereis(dbg), [dictionary]) ]),
    ?INFO("------------------"),
    State2 = do_tracing(State, Node, Cookie),
    true = erlang:monitor_node(Node, true),
    case State2#?STATE.connect_attempt_ref of
        undefined -> ok;
        TRef      -> timer:cancel(TRef)
    end,
    State2.

do_tracing(State, Node, Cookie) ->
    case node() of
        Node ->
            State;
        _ ->
            add_node(Node),
            {ok, MatchDesc} = dbg:p(all, call),
            ?INFO("dbg:p(all, call) -> ~p", [MatchDesc]),
            ?INFO("dbg nodes-------"),
            dbg:ln(),
            ?INFO("----------------"),
            %% Re-enable tracing on remote node:
            case goanna_db:lookup([trc_pattern, Node, Cookie]) of
                [] ->
                    State;
                TracePatternRecs ->
                    FRec = fun({Key, TrcPatternList}) -> TrcPatternList end,
                    TracePatterns = lists:flatten(lists:map(FRec, TracePatternRecs)),
                    F =
                    fun
                         % {{trace1@rpmbp,trace},[{trc_pattern,mnesia,undefined,undefined}]}
                        (#trc_pattern{m=Module,f=undefined,a=undefined}) ->
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

add_node(Node) ->
    case dbg:n(Node) of
        {ok, Node} ->
            ?INFO("dbg:n -> {ok,~p}", [Node]),
            ok;
        {error,{already_started,_}} ->
            ?INFO("dbg:n -> {error,{already_started,_}}");
        {error,{nodedown,Node}} ->
            ?INFO("dbg:n -> {error,{nodedown,~p}}", [Node])
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, #?STATE{ node=Node } = State) ->
    ?INFO("Node:~p down...", [Node]),
    {noreply, reconnect(State)};
handle_info(reconnect, #?STATE{ node = _Node } = State) ->
    ?INFO("reconnecting..."),


    % goanna_tracer:tracer(),
    % SysConfNodes = application:get_env(goanna, nodes, []),
    % lists:foreach(fun
    %     ({N, _Cookie}) when N == node() ->
    %         ?INFO("NOT ! RE-ADDING ~p dbg:n/1", [N]);
    %     ({OtherNode, _Cookie}) ->
    %         ?INFO("RE-ADDING ~p dbg:n/1", [OtherNode]),
    %         add_node(OtherNode)
    % end, SysConfNodes),


    {ok, NewState} = est_rem_conn(State, reconnect),
    {noreply, NewState};
handle_info(stop_trace, State) ->
    ok = dbg:stop_clear(),
    % {noreply, State#?STATE{tracing = false,
    %                        trace_module = undefined}};
    {stop,stop_trace,State};
handle_info({trace, Module}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    true = goanna_db:store([trace_pattern, Node, Cookie],
        #trc_pattern{m=Module}),
    enable_tracing([Module]),
    {noreply, State#?STATE{ tracing = true }};
handle_info({trace, Module, Function}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    true = goanna_db:store([trace_pattern, Node, Cookie],
        #trc_pattern{m=Module,f=Function}),
    enable_tracing([Module]),
    {noreply, State#?STATE{ tracing = true }};
handle_info({trace, Module, Function, Arity}, #?STATE{ node = Node, cookie = Cookie } = State) ->
    true = goanna_db:store([trace_pattern, Node, Cookie],
        #trc_pattern{m=Module,f=Function,a=Arity}),
    enable_tracing([Module]),
    {noreply, State#?STATE{ tracing = true }};

handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?INFO("Info ~p : ~p", [goanna_sup:id(Node, Cookie), Info]),
    {noreply, State}.

enable_tracing(T=[Module]) ->
    ?INFO("enable_tracing:~p~n", [T]),

    % {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    {ok, MatchDesc} = dbg:tpl(Module, cx),
    ?INFO("tpl:~p", [MatchDesc]);
enable_tracing(T=[Module, Function]) ->
    ?INFO("enable_tracing:~p~n", [T]),

    % {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, cx]),
    {ok, MatchDesc} = dbg:tpl(Module, Function, cx),
    ?INFO("tpl:~p", [MatchDesc]);
enable_tracing(T=[Module, Function, Arity]) ->
    ?INFO("enable_tracing:~p~n", [T]),

    % {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, Function, Arity, cx]),
    {ok, MatchDesc} = dbg:tpl(Module, Function, Arity, cx),
    ?INFO("tpl:~p", [MatchDesc]).

terminate(_Reason, #?STATE{node = Node, cookie = Cookie} = _State) ->
    true = goanna_db:terminate_node([Node, Cookie]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reconnect(State) ->
    {ok, TRef} = timer:send_after(250, reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef}.