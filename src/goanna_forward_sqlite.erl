-module(goanna_forward_sqlite).

-behaviour(goanna_forward_callback_mod).
-export([
    forward_init/1,
    forward/2
]).

-spec forward_init(ChildId :: atom()) -> {ok, pid() | atom()} | {error, term()}.
forward_init(_ChildId) ->
    {ok, self()}.

%% ----------

-spec forward(Process :: pid() | atom(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(_Process, {_, _TraceItem}) ->
    ok.