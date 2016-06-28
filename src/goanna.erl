-module(goanna).

-behaviour(gen_server).
-export([start/0,
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

name(Node, Cookie) ->
    goanna_sup:id(Node, Cookie).

start_link(Node, Cookie) ->
    gen_server:start_link({local, name(Node, Cookie)},
                          ?MODULE, {Node, Cookie}, []).

init({Node, Cookie}) ->
    true = ets:insert(nodelist, {Node,Cookie}),
    case est_rem_conn(#?STATE{node=Node, cookie=Cookie}) of
        {ok, State} ->
            ok = do_tracing(Node),
            true = erlang:monitor_node(Node, true),
            case State#?STATE.connect_attempt_ref of
                undefined -> ok;
                TRef      -> timer:cancel(TRef)
            end,
            {ok, State#?STATE{ connect_attempt_ref = undefined }};
        R ->
            R
    end.

est_rem_conn(#?STATE{ node=Node, cookie=Cookie } = State) ->
    case ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))) of
        true ->
            {ok, State#?STATE{connected=true}};
        false ->
            {ok, reconnect(State)}
    end.

do_tracing(Node) ->
    case node() of
        Node ->
            ok;
        _ ->

            % try, because, could already be started ....
            % try

            %     RD = rpc:call(Node, dbg, start, []),
            %     ?INFO("RD:~p~n", [RD])
            % catch
            %     C:E ->
            %         ok
            % end,

            % ok = gen_server:call(goanna_tracer, {tracer, Node}),
            % rpc:call(Node, dbg, stop, []),

            % TraceFn = fun (Trace, _) ->
            %             io:format(".", [Trace])
            %         end,

            % FunStr = "fun(Trace, _) -> io:format(\"trace\") end.",
            % {ok, Tokens, _} = erl_scan:string(FunStr),
            % {ok, [Form]} = erl_parse:parse_exprs(Tokens),
            % Bindings = erl_eval:add_binding('B', 2, erl_eval:new_bindings()),
            % {value, TraceFn, _} = erl_eval:expr(Form, Bindings),

            case dbg:n(Node) of
                {ok, Node} ->
                    ok;
                {error,{already_started,_}} ->
                    ok
            end,

            % RT = rpc:call(Node, dbg, tracer, [process, {TraceFn, ok}]),
            % RT = rpc:call(Node, dbg, tracer, []),
            % ?INFO("RT:~p~n", [RT]),
            {ok, MatchDesc} = dbg:p(all, call),
            % {ok, MatchDesc} = dbg:p(all, [c, timestamp]),
            % A = rpc:call(Node, dbg, p, [all, [c, timestamp]]),
            % ?INFO("remote p: ~p\n", [A]),
            ok
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, #?STATE{ node=Node } = State) ->
    ?INFO("Node:~p down...~n~n", [Node]),
    {noreply, reconnect(State)};
handle_info(reconnect, State) ->
    ?INFO("reconnect..."),
    {ok, NewState} = est_rem_conn(State),
    {noreply, NewState};
handle_info(stop_trace, State) ->
    ok = dbg:stop_clear(),
    % {noreply, State#?STATE{tracing = false,
    %                        trace_module = undefined}};
    {stop,stop_trace,State};
handle_info({trace, Module}, #?STATE{ node = _Node } = State) ->
    {ok, MatchDesc} = dbg:tpl(Module, cx),
    % {ok, MatchDesc} = rpc:call(Node, dbg, tpl, [Module, cx]),
    {noreply, State#?STATE{ tracing = true,
                            trace_module = Module,
                            matchdesc = MatchDesc
                          }
    };
handle_info(Info, #?STATE{ node=Node, cookie=Cookie } = State) ->
    ?INFO("Info ~p : ~p~n", [name(Node, Cookie), Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reconnect(State) ->
    {ok, TRef} = timer:send_after(250, reconnect),
    State#?STATE{connected=false, connect_attempt_ref=TRef}.