-module(goanna_forward_crash_mod).

%% @doc module is meant to crash when calling forward_init 
%% @end 

-behaviour(goanna_forward_callback_mod).
-export([
    forward_init/1,
    forward/2
]).

-include_lib("goanna.hrl").

-spec forward_init(atom()) -> {ok, pid() | atom()} | {error, term()}.
forward_init(_ChildId) ->
    throw({crash, on, purpose}).

-spec forward(_Process :: pid() | atom(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(_Process, {_Ts, _Node, _TraceItem}) ->
    ok.