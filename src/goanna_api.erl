-module (goanna_api).
-export([
    start/0,
    add_node/3,
    remove_node/1
]).

-export([
    trace/1, trace/2, trace/3, trace/4,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3
]).

-export([store_trace/2]).

-include_lib("goanna.hrl").

%%------------------------------------------------------------------------
%% API

%% Helper only
start() ->
    [
        application:start(APP)
        || APP <-
        [asn1, crypto, public_key, ssl, compiler, inets, syntax_tools, sasl,
         goldrush, lager, goanna]
    ].

add_node(Node, Cookie, Type) ->
	goanna_sup:start_child(Node, Cookie, Type).

remove_node(Node) ->
	goanna_sup:delete_child(Node).

% TODO: COMPLETE!!!!!!!
% update_default_trace_options() ->
%     application:set_env(goanna,

trace(Module) ->
    cluster_foreach_pidbang({trace, [{trc, #trc_pattern{m=Module}}]}).

trace(Module, Function) ->
    cluster_foreach_pidbang({trace, [{trc, #trc_pattern{m=Module,f=Function}}]}).

trace(Module, Function, Arity) ->
    cluster_foreach_pidbang({trace, [{trc, #trc_pattern{m=Module,f=Function,a=Arity}}]}).

trace(Module, Function, Arity, Opts) ->
    cluster_foreach_pidbang({trace, Opts++[{trc, #trc_pattern{m=Module,f=Function,a=Arity}}]}).

stop_trace() ->
    cluster_foreach_call(stop_all_trace_patterns).

stop_trace(Module) ->
    cluster_foreach_pidbang({stop_trace, #trc_pattern{m=Module}}).

stop_trace(Module, Function) ->
    cluster_foreach_pidbang({stop_trace, #trc_pattern{m=Module,f=Function}}).

stop_trace(Module, Function, Arity) ->
    cluster_foreach_pidbang({stop_trace, #trc_pattern{m=Module,f=Function,a=Arity}}).

cluster_foreach_call(Msg) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            call_node(Node, Cookie, Msg)
        end, ets:tab2list(nodelist)
    ).

cluster_foreach_pidbang(Msg) ->
    lists:foreach(
        fun({Node, Cookie, _Type}) ->
            pidbang_node(Node, Cookie, Msg)
        end, ets:tab2list(nodelist)
    ).

pidbang_node(Node, Cookie, Msg) ->
    whereis(goanna_sup:id(Node, Cookie)) ! Msg.

call_node(Node, Cookie, Msg) ->
    gen_server:call(goanna_sup:id(Node, Cookie), Msg).

%%------------------------------------------------------------------------

store_trace([trace, Node, Cookie],Trace) ->
    pidbang_node(Node, Cookie, {trace_item}),
    goanna_db:store([trace, Node, Cookie], Trace).