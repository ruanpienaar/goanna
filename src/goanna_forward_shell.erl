-module(goanna_forward_shell).

-behaviour(goanna_forward_callback_mod).
-export([
    forward_init/1,
    forward/2
]).

-include_lib("goanna.hrl").

-spec forward_init(atom()) -> {ok, pid() | atom()} | {error, term()}.
forward_init(_ChildId) ->
    ok.

-spec forward(_Process :: pid() | atom(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(_Process, {_Ts, Node, TraceItem}) ->
    io:format(goanna_common:format_trace_item(Node, TraceItem)).