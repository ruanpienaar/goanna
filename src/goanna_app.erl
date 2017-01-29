-module(goanna_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include_lib("goanna.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    goanna_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
