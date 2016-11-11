-module(goanna).
-export([main/1,
         forward/2
         ]).
%%! -smp disable +A 1 -sname goanna_script -hidden -setcookie goanna -config sys.config

-mode(compile).

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
    goanna_api:start(),
    ok = application:set_env(goanna, data_retrival_method, {push, 250, ?MODULE}),
    {goanna, GoannaConfig} = lists:keyfind(goanna, 1, Terms),
    case check_lookup_value(nodes, GoannaConfig) of
        [] ->
            io:format("No nodes to start, check sys.config", []);
        Nodes ->
            case check_lookup_value(traces, GoannaConfig) of
                [] ->
                    io:format("No traces to start, check sys.config", []);
                Traces ->
                    % io:format("Starting up nodes~n~p~n", [Nodes]),
                    startup_nodes(Nodes),
                    % io:format("Applying Traces ~p~n", [Traces]),
                    traces(Traces)
            end
    end,
    infinite_loop().
    % Also a way of starting a shell, from escript..
    % shell:start(),
    % timer:sleep(infinity).

% TODO: Maybe prompt user for input when list empty?
% H
% [{node,'p1@127.0.0.1'},{cookie,pasture},{type,tcpip_port}]
startup_nodes([]) ->
    ok;
startup_nodes([H|T]) ->
    Node   = check_lookup_value(node, H),
    Cookie = check_lookup_value(cookie, H),
    Type   = check_lookup_value(type, H),
    {ok, _} = goanna_api:add_node(Node, Cookie, Type),
    startup_nodes(T).

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
        	io:format("Trace ~p", [{Module}]),
            ok = goanna_api:trace(Module);
        {function, Function} ->
            case lists:keyfind(arity, 1, H) of
                false ->
                	io:format("Trace ~p", [{Module, Function}]),
                    ok = goanna_api:trace(Module, Function);
                {arity, Arity} ->
                	io:format("Trace ~p", [{Module, Function, Arity}]),
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
    % ets:i(),
    timer:sleep(1000),
    infinite_loop().

forward(_Tbl, {_Now, TraceItem}) ->
    io:format(standard_io, "~p~n", [format_trace_item(TraceItem)]).

format_trace_item(Trace={trace, _Pid, _Label, _Info}) ->
    Trace;
format_trace_item(Trace={trace, _Pid, _Label, _Info, _Extra}) ->
    Trace;
format_trace_item(Trace={trace_ts, _Pid, _Label, _Info, _ReportedTS}) ->
    Trace;
format_trace_item(Trace={trace_ts, _Pid, _Label, _Info, _Extra, _ReportedTS}) ->
    Trace;
format_trace_item(Trace={seq_trace, _Label, _SeqTraceInfo}) ->
    Trace;
format_trace_item(Trace={seq_trace, _Label, _SeqTraceInfo, _ReportedTS}) ->
    Trace;
format_trace_item(Trace={drop, _NumberOfDroppedItems}) ->
    Trace.

