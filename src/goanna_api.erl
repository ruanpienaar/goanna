-module (goanna_api).
-export([
    start/0,
    add_node/3,
    remove_node/1,
    nodes/0
]).

-export([
    trace/1, trace/2, trace/3, trace/4,
    stop_trace/0, stop_trace/1, stop_trace/2, stop_trace/3
]).

-export([store_trace/2]).
-define(GOANNA_SUP, goanna_sup).

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

nodes() ->
        [Name || {Name, _, _, _} <- supervisor:which_children(?GOANNA_SUP)].

-spec add_node(atom(), atom(), erlang_distribution | file | tcpip_port) -> {ok, pid()}.
add_node(Node, Cookie, Type) ->
	?GOANNA_SUP:start_child(Node, Cookie, Type).

remove_node(Node) ->
	?GOANNA_SUP:delete_child(Node).

% TODO: COMPLETE!!!!!!!
% update_default_trace_options() ->
%     application:set_env(goanna,

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
%     whereis(?GOANNA_SUP:id(Node, Cookie)) ! Msg.

call_node(Node, Cookie, Msg) ->
    gen_server:call(?GOANNA_SUP:id(Node, Cookie), Msg).

%%------------------------------------------------------------------------

store_trace([trace, Node, Cookie],Trace) ->
    ok = call_node(Node, Cookie, {trace_item, Trace}).
