-module(goanna_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include_lib("goanna.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    goanna_sup:start_link().

stop(_State) ->
    % Stop all traces! 
    goanna_api:stop_trace(),
    ok.
