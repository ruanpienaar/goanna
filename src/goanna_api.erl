-module (goanna_api).

%% Control Api
-export([
    start/0,
    add_node/3,
    remove_node/1,
    nodes/0,
    update_default_trace_options/1
]).

%% Trace Api
-export([
    trace/1, trace/2, trace/3, trace/4,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3,
    clear_all_traces/0,
    recv_trace/2
]).

-include_lib("goanna.hrl").

%%------------------------------------------------------------------------
%% API

%% Goanna start-up helper only
start() ->
    [
        application:start(APP)
        || APP <-
        [asn1, crypto, public_key, ssl, compiler, inets, syntax_tools, sasl,
         goldrush, lager, goanna]
    ].

nodes() ->
    [ChildId || {ChildId, _, _, _} <- supervisor:which_children(goanna_node_sup)].

-spec add_node(atom(), atom(), erlang_distribution | file | tcpip_port) -> {ok, pid()}.
add_node(Node, Cookie, Type) when Type == erlang_distribution;
                                  Type == file;
                                  Type == tcpip_port ->
	goanna_node_sup:start_child(Node, Cookie, Type).

remove_node(Node) ->
	goanna_node_sup:delete_child(Node).

update_default_trace_options(Opts) ->
    {ok, DefaultOpts} = application:get_env(goanna, default_trace_options),
    F = fun
        ({time, Time}, DefOpts) when is_integer(Time) ->
            lists:keystore(time, 1, DefOpts, {time, Time});
        ({messages, Msgs}, DefOpts) when is_integer(Msgs) ->
            lists:keystore(messages, 1, DefOpts, {messages, Msgs})
    end,
    NewDefaultOpts = lists:foldl(F, DefaultOpts, Opts),
    application:set_env(goanna, default_trace_options, NewDefaultOpts),
    cluster_foreach_call({update_state}).

trace(Module) ->
    cluster_foreach_call({trace, [{trc, #trc_pattern{m=Module}}]}).

trace(Module, Function) ->
    cluster_foreach_call({trace, [{trc, #trc_pattern{m=Module,f=Function}}]}).

trace(Module, Function, Arity) ->
    cluster_foreach_call({trace, [{trc, #trc_pattern{m=Module,f=Function,a=Arity}}]}).

trace(Module, Function, Arity, Opts) ->
    cluster_foreach_call({trace, Opts++[{trc, #trc_pattern{m=Module,f=Function,a=Arity}}]}).

stop_trace() ->
    cluster_foreach_call(stop_all_trace_patterns).

stop_trace(Module) ->
    cluster_foreach_call({stop_trace, #trc_pattern{m=Module}}).

stop_trace(Module, Function) ->
    cluster_foreach_call({stop_trace, #trc_pattern{m=Module,f=Function}}).

stop_trace(Module, Function, Arity) ->
    cluster_foreach_call({stop_trace, #trc_pattern{m=Module,f=Function,a=Arity}}).

clear_all_traces() ->
    cluster_foreach_call({clear_db_traces}).

cluster_foreach_call(Msg) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            call_node(Node, Cookie, Msg)
        end, ets:tab2list(nodelist)
    ).

% cluster_foreach_pidbang(Msg) ->
%     lists:foreach(
%         fun({Node, Cookie, _Type}) ->
%             pidbang_node(Node, Cookie, Msg)
%         end, ets:tab2list(nodelist)
%     ).

% pidbang_node(Node, Cookie, Msg) ->
%     whereis(goanna_node_sup:id(Node, Cookie)) ! Msg.

% call_node(ChildId, Msg) ->
%     gen_server:call(ChildId, Msg).

%% TODO: HANDLE TIMEOUT
call_node(ChildId, Msg) ->
    gen_server:call(ChildId, Msg).
call_node(Node, Cookie, Msg) ->
    gen_server:call(goanna_node_sup:id(Node, Cookie), Msg).

% pidbang_trace_collector(Msg) ->
%     whereis(goanna_trace_collector) ! Msg,
%     ok.

%%------------------------------------------------------------------------
recv_trace([trace, _ChildId], end_of_trace) ->
    %% TODO: MAYBE check if traces are disabled ??? on goanna_node?
    ok;
recv_trace([trace, ChildId], Trace={trace, _Pid, _Label, _Info}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace});
    call_node(ChildId, {trace_item, Trace});

recv_trace([trace, ChildId], Trace={trace, _Pid, _Label, _Info, _Extra}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace});
    call_node(ChildId, {trace_item, Trace});

recv_trace([trace, ChildId], Trace={trace_ts, _Pid, _Label, _Info, _ReportedTS}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace});
    call_node(ChildId, {trace_item, Trace});

recv_trace([trace, ChildId], Trace={trace_ts, _Pid, _Label, _Info, _Extra, _ReportedTS}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace});
    call_node(ChildId, {trace_item, Trace});

recv_trace([trace, _ChildId], Trace={seq_trace, _Label, _SeqTraceInfo}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace});
    % call_node(ChildId, {trace_item, Trace});
    ?EMERGENCY("! Trace Message ~p not implemented yet !", [Trace]),
    ok;
recv_trace([trace, _ChildId], Trace={seq_trace, _Label, _SeqTraceInfo, _ReportedTS}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace});
    % call_node(ChildId, {trace_item, Trace});
    ?EMERGENCY("! Trace Message ~p not implemented yet !", [Trace]),
    ok;

recv_trace([trace, _ChildId], Trace={drop, _NumberOfDroppedItems}) ->
    % pidbang_trace_collector({trace_item, ChildId, Trace}).
    % call_node(ChildId, {trace_item, Trace});
    ?EMERGENCY("! Trace Message ~p not implemented yet !", [Trace]),
    ok.