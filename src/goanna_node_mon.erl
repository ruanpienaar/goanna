-module(goanna_node_mon).
-export([
    start_link/0,
    monitor/1
]).

start_link() ->
    {ok, proc_lib:spawn_link(fun() ->
        register(?MODULE, self()),
        loop()
    end)}.

monitor(NodePid) ->
    ?MODULE ! {monitor, NodePid}.

loop() ->
    receive
        {monitor, NodePid} ->
            io:format("Going to monitor ~p~n", [NodePid]),
            Ref = erlang:monitor(process, NodePid),
            io:format("~p monitor reference ~p~n", [?MODULE, Ref]),
            loop();
        A ->
            io:format("[~p] received ~p~n", [?MODULE, A]),
            loop()
    end.