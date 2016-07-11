-module(goanna_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include_lib("goanna.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Start=goanna_sup:start_link(),

    % ?DEBUG("DEBUG! ~p ", [?MODULE]),
    % ?WARNING("WARNING! ~p ", [?MODULE]),
    % ?INFO("INFO! ~p ", [?MODULE]),
    % ?ERROR("ERROR! ~p ", [?MODULE]),
    % ?CRITICAL("CRITICAL! ~p ", [?MODULE]),
    % ?ALERT("ALERT ~p ", [?MODULE]),
    % ?EMERGENCY("EMERGENCY! ~p ", [?MODULE]),

    Start.

stop(_State) ->
    ok.
