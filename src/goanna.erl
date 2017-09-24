-module(goanna).

-export([main/1]).

-mode(compile).

-include_lib("goanna.hrl").

-spec main(term()) -> term().
main([]) ->
    main(["-s"]);
main([NameType]) when NameType =:= "-s";
                      NameType =:= "-l" ->
    %% FOLLOW the below if no arguments were given....
    {ok, [Terms]} = file:consult("sys.config"),
    ok = application:set_env(kakapo, event_handler, []),
    %% TODO: Use Getopt
    case NameType of
        "-s" ->
	        {ok,_} = net_kernel:start([somename, shortnames]);
    	"-l" ->
	        {ok,_} = net_kernel:start([somename, longnames])
    end,
    ok = application:set_env(hawk, conn_retry_wait, 100),
    ok = application:set_env(hawk, connection_retries, 1000),
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
    {ok,_} = goanna_api:start(),
    case check_lookup_value(nodes, GoannaConfig) of
        [] ->
            io:format("No nodes to start in ./sys.config", []),
            timer:sleep(50),
            erlang:halt(0);
        _Nodes ->
            case check_lookup_value(traces, GoannaConfig) of
                [] ->
                    io:format("No traces to start in ./sys.config", []),
                    timer:sleep(100),
                    erlang:halt(0);
                Traces ->
                    timer:sleep(100),
                    traces(Traces)
            end
    end,
    infinite_loop().

check_lookup_value(Key, Proplist) ->
    case lists:keyfind(Key, 1, Proplist) of
        false    -> throw({missing_value, Key, Proplist});
        {Key, V} -> V
    end.

infinite_loop() ->
    timer:sleep(1000),
	infinite_loop().

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