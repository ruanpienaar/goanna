-module(goanna_forward_only).

%% @doc module is meant to test forward module that only has forward/2
%% @end 

-include_lib("eunit/include/eunit.hrl").

% -behaviour(goanna_forward_callback_mod). % See @doc 
-export([
    forward/2
]).

-spec forward(_Process :: pid() | atom(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(_Process, {_Ts, _Node, _TraceItem}) ->
    ok.