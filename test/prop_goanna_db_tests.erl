-module(prop_goanna_db_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("goanna.hrl").

%% ---------
%% init_node/1

prop_init_node() ->
    % init goanna_db
    ok = goanna_db:init(),
    ?FORALL(
        Node,
        atom(),
        ?FORALL(
            Cookie,
            atom(),
            ?IMPLIES(valid_node_cookie(Node, Cookie), do_init_node(Node, Cookie))
        )
    ).

do_init_node(Node, Cookie) ->
    Type = tcpip_port,
    ChildId = goanna_node_sup:id(Node, Cookie),
    Res = ({ok, goanna_db:create_node_obj(Node, Cookie, Type)}
            == goanna_db:init_node([Node, Cookie, Type, ChildId])),
    true = ets:delete(ChildId),
    Res.

valid_node_cookie(N, _) when N == '' orelse
                             N == '?' orelse
                             N == ?NODE_COOKIE_SEP ->
    false;
valid_node_cookie(_N, _) ->
    true.

%% ---------
%% all/1

prop_goanna_db_all() ->
    ?FORALL(
        R,
        atom(),
        valid_response(goanna_db:all(R))
    ).

valid_response({error, unknown_table}) ->
    true;
valid_response(R) when is_list(R) ->
    true;
valid_response(_) ->
    false.

