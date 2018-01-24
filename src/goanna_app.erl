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
    case goanna_sup:start_link() of
        {ok, SupPid} ->
            goanna_db:init(),
            %% Sys.config nodes being added below:
            ok = lists:foreach(fun({Node, Cookie}) ->
                ChildId = goanna_node_sup:id(Node, Cookie),
                %% Sys.config traces being added below:
                ok = lists:foreach(fun
                    (TrcPattern) when is_tuple(TrcPattern) andalso
                                    ( size(TrcPattern)==1 orelse
                                      size(TrcPattern)==2 orelse
                                      size(TrcPattern)==3 ) ->
                        true = goanna_db:store_trace_pattern(ChildId, [TrcPattern], []);
                    (TrcPattern) when is_list(TrcPattern) -> %% TODO: maybe add a is_string() check?
                        {{M,F,A},MatchSpec,[_Flag]} = redbug_msc:transform(TrcPattern),
                        true = goanna_db:store_trace_pattern(ChildId, [{M,F,A,MatchSpec}], [])
                end, application:get_env(goanna, traces, [])),
                {ok, _} = goanna_api:add_node(Node, Cookie)
            end, application:get_env(goanna, nodes, [])),
            {ok, SupPid};
        X ->
            X
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.