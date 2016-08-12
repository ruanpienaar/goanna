-module(goanna).
-export([main/1,
         forward/2
         ]).
%%! -smp enable -sname goanna_script -hidden -setcookie goanna -config sys.config

-mode(compile).

main([]) ->

    %% No Args flow
    %% FOLLOW the below if no arguments were given....

    % io:format("init args ~p~n", [init:get_arguments()]),
    % io:format("Args ~p~n", [Args]),
    % io:format("~p~n", [file:list_dir(".")]).

    {ok, [Terms]} = file:consult("sys.config"),
    % ok = application:set_env(lager, log_root, "log/"),
    % ok = application:set_env(lager, handlers, [
    %     {lager_console_backend, debug},
    %     {lager_file_backend, [{file, "error.log"}, {level, error}]},
    %     {lager_file_backend, [{file, "console.log"}, {level, info}]}
    % ]),


    {ok,_} = net_kernel:start([somename]),
    goanna_api:start(),

    % {data_retrival_method, {push, 250, goanna_shell_printer}}
    application:set_env(goanna, data_retrival_method, {push, 250, ?MODULE}),

    % io:format("~n ~p ~n", [application:get_all_env(lager)]),



    % io:format("~p~n", [Terms]).
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
    % ets:i(),
    timer:sleep(1000),
    infinite_loop().

forward(Tbl, {Now, TraceItem}) ->
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

