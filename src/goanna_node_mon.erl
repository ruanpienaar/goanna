-module(goanna_node_mon).

-export([
    start_link/0,
    monitor/2
]).

start_link() ->
    {ok, proc_lib:spawn_link(fun() ->
        true = register(?MODULE, self()),
        loop([])
    end)}.

monitor(Node, NodePid) ->
    ?MODULE ! {monitor, Node, NodePid}.

loop(Nodes) ->
    receive
        {monitor, Node, NodePid} ->
            % io:format("Going to monitor ~p~n", [NodePid]),
            loop(case lists:keyfind(Node, 1, Nodes) of
                    false ->
                        [node_entry(Node, NodePid) | Nodes];
                    {Node, _SomePid, Ref} ->
                        true = erlang:demonitor(Ref),
                        lists:keyreplace(Node, 1, Nodes, node_entry(Node, NodePid))
                 end);
        D = {'DOWN', _, process, NodePid, Reason}
                when Reason == normal orelse
                     Reason == {remote_node_down} ->
            case lists:keyfind(NodePid, 2, Nodes) of
                {Node, NodePid, _Ref} ->
                    loop(lists:keydelete(Node, 1, Nodes));
                false ->
                    io:format("[~p] unknown DOWN ~p~n", [?MODULE, D]),
                    loop(Nodes)
            end;
        A ->
            io:format("[~p] received UNKNOWN message: ~p~n", [?MODULE, A]),
            loop(Nodes)
    end.

node_entry(Node, NodePid) ->
    %io:format("~p monitor reference ~p~n", [?MODULE, Ref]),
    {Node, NodePid, erlang:monitor(process, NodePid)}.