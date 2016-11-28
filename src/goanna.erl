-module(goanna).
-export([main/1]).
%%! -smp disable +A 1 -sname goanna_script -hidden -setcookie goanna -config sys.config
-mode(compile).
-include_lib("goanna.hrl").

main([]) ->
    main(["-s"]);

main([NameType]) when NameType =:= "-s";
                      NameType =:= "-l" ->
    %% FOLLOW the below if no arguments were given....
    {ok, [Terms]} = file:consult("sys.config"),
    %% TODO: Use Getopt
    case NameType of
        "-s" ->
	        {ok,_} = net_kernel:start([somename, shortnames]);
    	"-l" ->
	        {ok,_} = net_kernel:start([somename, longnames])
    end,
    ok = application:set_env(hawk, conn_retry_wait, 10),
    ok = application:set_env(hawk, connection_retries, 10000),
    %% Force push!
    ok = application:set_env(goanna, data_retrival_method, {push, 100, goanna_shell_printer}),
    {goanna, GoannaConfig} = lists:keyfind(goanna, 1, Terms),
    case lists:keyfind(default_trace_options, 1, GoannaConfig) of
        false ->
        	ok;
        {default_trace_options,DTO} ->
        	application:set_env(goanna, default_trace_options, DTO)
    end,
    case lists:keyfind(dbg_p_flags,1, GoannaConfig) of
    	false ->
    		application:set_env(goanna, dbg_p_flags, call);
    	{dbg_p_flags, DPF} ->
    		application:set_env(goanna, dbg_p_flags, DPF)
    end,
    {lager, LagerConf} = lists:keyfind(lager, 1, Terms),
    lists:foreach(fun({LagerConfCol,LagerConfVal}) ->
    	application:set_env(lager, LagerConfCol, LagerConfVal)
    end, LagerConf),	
    true = lists:all(fun(ok) -> true; (_) -> false end, goanna_api:start()),
    case check_lookup_value(nodes, GoannaConfig) of
        [] ->
            ?CRITICAL("No nodes to start, check sys.config", []),
            timer:sleep(10),
            erlang:halt(0);
        Nodes ->
            case check_lookup_value(traces, GoannaConfig) of
                [] ->
                    ?CRITICAL("No traces to start, check sys.config", []),
                    timer:sleep(10),
                    erlang:halt(0);
                Traces ->
                    ok = startup_nodes(Nodes),
                    ?DEBUG("Waiting for nodes......~n", []),
                    wait_for_nodes(Nodes, 50),
                    timer:sleep(50),
                    goanna_api:stop_trace(),
                    ?DEBUG("Applying Traces ~p~n", [Traces]),
                    traces(Traces)
            end
    end,
    infinite_loop().
    % Also a way of starting a shell, from escript..
    %%shell:start(),
    %%timer:sleep(infinity).

% TODO: Maybe prompt user for input when list empty?
% H
% [{node,'p1@127.0.0.1'},{cookie,pasture},{type,tcpip_port}]
startup_nodes([]) ->
    ok;
startup_nodes([H|T]) ->
    Node   = check_lookup_value(node, H),
    ?DEBUG("Starting ~p~n", [Node]),
    Cookie = check_lookup_value(cookie, H),
    Type   = check_lookup_value(type, H),
    {ok, _} = goanna_api:add_node(Node, Cookie, Type),
    startup_nodes(T).

wait_for_nodes(Nodes, 0) ->
	timer:sleep(10),
	?CRITICAL("Wait was too long, nodes haven't arrived", []),
	erlang:halt(0);
wait_for_nodes(Nodes, Count) ->
	GN = goanna_api:nodes(),
	?DEBUG("Nodes known to goanna: ~p~n", [Nodes]),
	case length(GN)==length(Nodes) of
		true ->
			?DEBUG("Nodes are alive now...~n", []),
			ok;
		false ->
			?DEBUG("Waiting for nodes....~n", []),
			timer:sleep(25),
			wait_for_nodes(Nodes, Count-1)
	end.

% H
% [{module,pasture_meetup}]
% [{module,pasture_meetup}, {function,handle_info}]
% [{module,pasture_meetup}, {function,handle_info}, {arity,2}]
traces([]) ->
    ok;
traces([H|T]) ->
    Module = check_lookup_value(module, H),
    case lists:keyfind(function, 1, H) of
        false ->
            ok = goanna_api:trace(Module);
        {function, Function} ->
            case lists:keyfind(arity, 1, H) of
                false ->
                    ok = goanna_api:trace(Module, Function);
                {arity, Arity} ->
                    ok = goanna_api:trace(Module, Function, Arity)
            end
    end,
    traces(T).

check_lookup_value(Key, Proplist) ->
    case lists:keyfind(Key, 1, Proplist) of
        false    -> throw({missing_value, Key, Proplist});
        {Key, V} -> V
    end.

infinite_loop() ->
    timer:sleep(1000),
    infinite_loop().