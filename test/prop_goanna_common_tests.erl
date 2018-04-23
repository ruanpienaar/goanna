-module(prop_goanna_common_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

% prop_prop_value() ->
%     ?FORALL(
%         X,
%         list({Key=term(), term()}),
%         true(X, goanna_common:prop_value(Key, X, default))
%     ).

    % ?FORALL(
    %     X,
    %     atom(),
    %     ?FORALL(
    %         Y,
    %         proplist(),
    %         ?FORALL(
    %             Z,
    %             any(),
    %             true(goanna_common:prop_value(X, Y, Z))
    %         )
    %     )
    % ).

% true(X, Resp) ->
%     io:format("X:  > ~p < ~n RESP: > ~p <~n", [X, Resp]),
%     true.

% test(X, Y) ->
%     io:format("~p ~p~n", [X, Y]),
%     true.
