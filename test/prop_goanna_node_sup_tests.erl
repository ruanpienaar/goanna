-module(prop_goanna_node_sup_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("goanna.hrl").


% TODO:
% Symetric function
% the   to_node(ChildID)->Node, Cookie   ==    id(Node, Cookie) -> ChildId


prop_id() ->
    ?FORALL([Node, Cookie], [atom(), atom()], id(Node, Cookie)).

id(Node, Cookie) ->
    R = goanna_node_sup:id(Node, Cookie),
    % ?debugFmt("Results ~p\n\n", [R]),
    is_atom(R).

prop_to_node() ->
    ?FORALL([Node, Cookie], [atom(), atom()],
        ?IMPLIES(valid_node_cookie(Node, Cookie), to_node(Node, Cookie))
    ).

valid_node_cookie(N, C) when  N == '' orelse N == '?' orelse
                              C == '' orelse C == '?' ->
    false;
valid_node_cookie(N, C) ->
    [CSEPINT] = io_lib:format("~w", ?NODE_COOKIE_SEP),
    case lists:member(CSEPINT, atom_to_list(N)) of
        true ->
            false;
        false ->
            case lists:member(CSEPINT, atom_to_list(C)) of
                true ->
                    false;
                false ->
                    %?debugFmt("VALID Node ~p Cookie ~p\n\n", [Node, Cookie]),
                    true
            end
    end.

to_node(Node, Cookie) ->
    %?debugFmt("Checking Node ~p Cookie ~p\n\n", [Node, Cookie]),
    ChildId = atom_to_list(Node)++?NODE_COOKIE_SEP++atom_to_list(Cookie),
    R = goanna_node_sup:to_node(ChildId),
    is_list(R) andalso
    length(R) == 2 andalso
    is_atom(lists:nth(1, R)) andalso
    is_atom(lists:nth(1, R)).