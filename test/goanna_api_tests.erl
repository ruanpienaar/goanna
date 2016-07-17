-module(goanna_api_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("goanna.hrl").

%% Find a smarter way of creating a remote node...
api_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
          {"API -> nodes/0", fun goanna_api_nodes/0}
        , {"API -> add_node/3", fun goanna_api_add_node/0}
        , {"API -> add_node/3 validation tests", fun goanna_api_add_node_validation/0}
        % , {"API -> remove_node/1", fun remove_node/0}
        , {"API -> remove_node/1 validation tests", fun remove_node_validation/0}
     ]
    }.

goanna_api_nodes() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes().

goanna_api_add_node() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    %% Adding it
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% Adding a duplicate
    {error,{already_started,GoannaNodePid}} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% removing it
    ok = goanna_api:remove_node(Node).

goanna_api_add_node_validation() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    %% Invalid data:
     {error, badarg} = goanna_api:add_node(1, 2, 3),
     {error, badarg} = goanna_api:add_node(atom, 2, 3),
     {error, badarg} = goanna_api:add_node(atom, atom, 3),
     {error, badarg} = goanna_api:add_node(atom, atom, fakeone).

remove_node() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("tests@"++Host),
    Cookie = cookie,
    GoannaNode_Cookie = list_to_atom(atom_to_list(Node)++"_"++atom_to_list(Cookie)),

    {error, no_such_node} = goanna_api:remove_node('fake@nohost'),

    % mmmm, the ets table is still there, ...
    % true=ets:delete('tests@rpmbp_cookie'),

    %% Adding it
    {ok, GoannaNodePid} =
        goanna_api:add_node(Node, cookie, erlang_distribution),
    [GoannaNode_Cookie] = goanna_api:nodes(),

    %% removing it
    ok = goanna_api:remove_node(Node).


remove_node_validation() ->
    %% THere should be no nodes, at first.
    [] = goanna_api:nodes(),
    {error, badarg} = goanna_api:remove_node(12345),
    {error, badarg} = goanna_api:remove_node("other").

%%------------------------------------------------------------------------

setup() ->
    [_,_,_,_,_,_,_,_,_,_,_] =
        goanna_api:start(),
    {ok, Host} = inet:gethostname(),
    make_distrib("tests@"++Host, shortnames),
    slave:start(Host, test1),
    ok.

cleanup(_) ->
    [_,_,_,_,_,_,_,_,_,_,_] =
        goanna_api:stop(),
    stop_distrib(),
    ok.

    -spec make_distrib( NodeName::string()|atom(), NodeType::shortnames | longnames) ->
        {ok, ActualNodeName::atom} | {error, Reason::term()}.
    make_distrib(NodeName, NodeType) when is_list(NodeName) ->
        make_distrib(erlang:list_to_atom(NodeName), NodeType);
    make_distrib(NodeName, NodeType) ->
        case node() of
            'nonode@nohost' ->
                [] = os:cmd("epmd -daemon"),
                case net_kernel:start([NodeName, NodeType]) of
                    {ok, _Pid} -> node()
                end;
            CurrNode ->
                CurrNode
        end.

    stop_distrib()->
        net_kernel:stop().
