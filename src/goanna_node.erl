-module(goanna_node).
-export([
    start_link/4
]).

-ifdef(TEST).
-export([
    initial_state/4,
    stop_all_traces_return_state/1
]).
-endif.

%% -----------------------------------------------------------------------------------
%% @doc
%%
%%  1) previous traces would be started only
%%  when the entire application restarts. This goanna_node should not
%%  crash, so we do not need to "restart" the traces when it starts again.
%%
%%  2) the remote code_server has to be up after hawk(net_kernel)
%%  was able to connect, todo: maybe make this configurable.
%%
%%  3) Trace total message count and/or trace time will be applied to all trace patterns.
%%     Updating the default trace options will trigger the re-trace with the new options, if tracing.
%%     Also adding new traces will restart the trace timer and trace count.
%%
%% @end
%% -----------------------------------------------------------------------------------

start_link(Node, Cookie, Type, ChildId) when is_atom(Node) andalso 
                                             is_atom(Cookie) andalso
                                             Type == tcpip_port andalso %% TODO: add bin_file type
                                             is_atom(ChildId) ->
    State = env_state(initial_state(Node, Cookie, Type, ChildId)),
    {ok, proc_lib:spawn_link(fun() ->
        goanna_log:log("goanna_node start_link ~p\n", [self()]),
        false = process_flag(trap_exit, true),
        goanna_node_mon:monitor(Node, self()),
        true = erlang:register(goanna_node_sup:id(Node, Cookie), self()),
        {ok, _} = goanna_db:init_node([Node, Cookie, Type, ChildId]),
        dbg_trace_steps(State)
    end)}.

initial_state(Node, Cookie, Type, ChildId) ->
    #{ node => Node,
       cookie => Cookie,
       type => Type,
       child_id => ChildId,
       trace_msg_count => 0,
       trace_timer_tref => undefined,
       trace_client_pid => undefined,
       tracing => false,
       push_timer_tref => undefined,
       previous_trace_client_pid => undefined
    }.

env_state(#{child_id := ChildId, node := Node} = State) ->
    DefaultTraceOpts =
        application:get_env(goanna, default_trace_options, []),
    MaxTraceTime =
        goanna_common:prop_value(time, DefaultTraceOpts, false),
    MaxMessages =
        goanna_common:prop_value(messages, DefaultTraceOpts, false),
    {FMethod, DataForwardProcess} =
        case application:get_env(goanna, data_retrival_method, pull) of
            pull ->
                {pull, undefined};
            F = {push, _Interval, Mod, _Amount} ->
                {ok, FPidOrName} = check_and_init_forward_mod(Node, ChildId, Mod),
                {F, FPidOrName}
        end,
    State#{
        trace_max_msg => MaxMessages,
        trace_max_time => MaxTraceTime,
        data_retrival_method => FMethod,
        data_forward_process => DataForwardProcess
    }.

-spec check_and_init_forward_mod(node(), atom(), atom()) -> ok | {ok, pid() | atom()}.
check_and_init_forward_mod(Node, ChildId, Mod) ->
    case c:l(Mod) of % Make sure the default is loaded...ugly...
        {module, Mod} ->
            % TODO: build a behaviour checker...
            case erlang:function_exported(Mod, forward, 2) andalso
                 erlang:function_exported(Mod, forward_init, 1)
            of
                true ->
                    try
                        {ok, _} = Mod:forward_init(ChildId)
                    catch
                        E:R ->
                            goanna_log:log("[~p] Forwarding callback module ~p "
                                " forward_init failed ~p ~p\n",
                                [?MODULE, Mod, E, R]),
                            goanna_api:remove_goanna_node(Node),
                            throw({forward_module, Mod, forward_init_crashed, E, R})
                    end;
                false ->
                    goanna_log:log("[~p] Forwarding callback module ~p "
                              "missing required behaviour functions...Removing ChildId ~p...",
                              [?MODULE, Mod, ChildId]),
                    goanna_api:remove_goanna_node(Node),
                    throw({Mod, not_goanna_forward_module_compatible})
            end;
        {error, nofile} ->
            goanna_api:remove_goanna_node(Node),
            throw({Mod, does_not_exist})
    end.

%% -----------------------------------------------------------------------------------
%% Running
% ! Make sure that the tracelist is cleared before falling into here..
dbg_trace_steps(#{
        node := Node,
        child_id := ChildId,
        data_retrival_method := FMethod,
        push_timer_tref := PTT } = State) ->
    case setup_trace_pid(State) of
        terminate ->
            terminate();
        State2 ->
            %% If there are tracelist items for this ChildId, then start the tracing with them...
            %% Usually left over from startup, when the node disconnected, and reconnected again...
            State3 =
                case goanna_db:lookup([tracelist, ChildId]) of
                    [] ->
                        State2;
                    TrcPatterns ->
                        ok = lists:foreach(fun(TrcPattern) ->
                            {ok, _} = enable_tracing(Node, TrcPattern)
                        end, TrcPatterns),
                        State2#{ tracing => true }

                end,
            loop(State3#{
                push_timer_tref => handle_data_retrival_method(PTT, FMethod)
            })
    end.

setup_trace_pid(#{
        node := Node,
        cookie := Cookie,
        type := Type,
        trace_client_pid := PrevTraceCLientPid
        } = State) ->
    goanna_log:log("setup_trace_pid ~p\n", [State]),
    case trace_steps(Node, Cookie, Type) of
        {error,{badrpc,nodedown}} ->
            terminate(State);
        {ok, RemoteDbgPid, TraceCLientPid} ->
            State#{
                remote_dbg_pid => RemoteDbgPid,
                trace_client_pid => TraceCLientPid,
                previous_trace_client_pid => PrevTraceCLientPid
            }
    end.

loop(#{ trace_msg_count := TMC,
        trace_max_msg := TMM
        } = State) when TMC >= TMM ->
    debug("stop all traces\n", []),
    dbg_trace_steps(stop_all_traces_return_state(State));
loop(#{ node := Node,
        cookie := Cookie,
        child_id := ChildId,
        trace_client_pid := TraceCLientPid,
        previous_trace_client_pid := PrevTraceCLientPid,
        trace_msg_count := TMC,
        trace_max_time := TMT,
        tracing := Tracing,
        trace_timer_tref := TT,
        push_timer_tref := PTT } = State) ->
    receive
        %% NoReply casts
        {trace_item, _Trace} when not Tracing ->
            goanna_log:log("{trace_item, _Trace} when not Tracing ->\n", []),
            loop(State);
        {trace_item, Trace} when Tracing ->
            goanna_log:log("{trace_item, Trace} when Tracing ->\n", []),
            true = goanna_db:store_trace(ChildId, Node, Trace),
            loop(State#{ trace_msg_count => TMC+1 });
        {push_data, Mod, Amount} ->
            goanna_log:log("{push_data, Mod, Amount} ->\n", []),
            %goanna_log:log("~p\n", [State]),
            #{data_forward_process := DWP} = State,
            ok = goanna_db:push(DWP, ChildId, Mod, Amount),
            loop(State);
        {stop_all_trace_patterns} when not Tracing ->
            goanna_log:log("{stop_all_trace_patterns} when not Tracing ->\n", []),
            loop(State);
        {stop_all_trace_patterns} when Tracing ->
            goanna_log:log("{stop_all_trace_patterns} when Tracing ->\n", []),
            NewState = stop_all_traces_return_state(State),
            dbg_trace_steps(NewState);
        %% Reply calls
        {{update_default_trace_options}, Reply} ->
            goanna_log:log("{{update_default_trace_options}, Reply} ->\n", []),
            %% Leave the existing traces, user has to manually stop them,
            %% all of the new traces will have the newly set default value.
            NewState = env_state(State),
            Reply ! ok,
            loop(NewState);
        {{update_data_retrival_method}, Reply} ->
            goanna_log:log("{{update_data_retrival_method}, Reply} ->\n", []),
            case PTT of
                % busy using "pull", so no push timer to cancel,
                undefined ->
                    ok;
                % Busy using "push" so cancel the timer, and
                PTT ->
                    cancel_timer(PTT)
            end,
            NewState = env_state(State),
            #{ data_retrival_method := FMethod } = NewState,
            NewState2 = NewState#{ push_timer_tref => handle_data_retrival_method(PTT, FMethod) },
            Reply ! ok,
            loop(NewState2);
        {{trace, TrcPatterns}, Reply} ->
            goanna_log:log("{{trace, TrcPatterns}, Reply} ->\n", []),
            cancel_timer(TT),
            % Start tracing TrcPatterns
            ok = trace(ChildId, TrcPatterns, Node),
            Reply ! ok,
            loop(State#{
                tracing => true,
                trace_msg_count => 0,
                trace_timer_tref => timer:send_after(TMT, {stop_all_trace_patterns})
            });

        % TODO: create a trace run for Custom trace opts, Num Messages and time
        %       irrespective of the system trace messages and time
        % {{trace, TrcOpts, TrcPatterns}, Reply} ->

        {{stop_trace, _TrcPattern}, Reply} when not Tracing ->
            goanna_log:log("{{stop_trace, _TrcPattern}, Reply} when not Tracing ->\n", []),
            Reply ! ok,
            loop(State);
        {{stop_trace, TrcPattern}, Reply} when Tracing ->
            goanna_log:log("{{stop_trace, TrcPattern}, Reply} when Tracing ->\n", []),
            cancel_timer(TT),
            true = goanna_db:delete_child_id_trace_pattern(Node, Cookie, TrcPattern),
            remote_dbg_ctpl(Node, TrcPattern),
            Reply ! ok,
            loop(State);
        {{stop_all_trace_patterns}, Reply} when not Tracing ->
            goanna_log:log("{{stop_all_trace_patterns}, Reply} when not Tracing ->\n", []),
            Reply ! ok,
            loop(State);
        {{stop_all_trace_patterns}, Reply} when Tracing ->
            goanna_log:log("{{stop_all_trace_patterns}, Reply} when Tracing ->\n", []),
            cancel_timer(TT),
            NewState = stop_all_traces_return_state(State),
            Reply ! ok,
            dbg_trace_steps(NewState);
        {'EXIT', TraceCLientPid, _Reason} ->
            goanna_log:log("{'EXIT', TraceCLientPid, _Reason} ->\n", []),
            % State2 = State#{ trace_client_pid => undefined },
            case setup_trace_pid(State) of
                terminate ->
                    terminate();
                State2 ->
                    loop(State2)
            end;
        {'EXIT', PrevTraceCLientPid, _Reason} ->
            goanna_log:log("{'EXIT', PrevTraceCLientPid, _Reason} ->\n", []),
            loop(State);
        {'EXIT', FromPid, shutdown} ->
            goanna_log:log("{'EXIT', FromPid, shutdown} ->\n", []),
            case whereis(goanna_node_sup) of
                FromPid ->
                    terminate = terminate(State),
                    terminate();
                _ ->
                    loop(State)
            end;
        {system, From, _Msg = get_state} ->
            goanna_log:log("{system, From, _Msg = get_state} ->\n", []),
            {_, _} = gen:reply(From, State),
            loop(State);
        {system, From, _Msg = get_status} ->
            goanna_log:log("{system, From, _Msg = get_status} ->\n", []),
            SysState = running,
            Parent = whereis(goanna_node_sup),
            Mod = ?MODULE,
            Name = ChildId,
            {_, _} = gen:reply(From, get_status(SysState, Parent, Mod, _Debug=false, _Misc=[Name, State, Mod, undefined, undefined])),
            loop(State);
        Any ->
            goanna_log:log("Any ->\n", []),
            goanna_log:log("UNEXPECTED ~p ~p loop recv : ~p~n", [self(), erlang:process_info(self(), registered_name), Any]),
            loop(State)
    end;
loop(State) ->
    goanna_log:log("WHY DID THIS HAPPEN ~p\n", [State]),
    loop(State).

terminate() ->
    receive
        X ->
            X
    end.

stop_all_traces_return_state(#{ node := Node, cookie := Cookie, type := Type, trace_timer_tref := TT} = State) ->
    ok = disable_all_tracing(Node),
    clear_tracelist(Node, Cookie, Type),
    ok = cancel_timer(TT),
    State#{ trace_msg_count => 0,
            trace_timer_tref => undefined,
            tracing => false
    }.

-spec handle_data_retrival_method(reference(), {push, non_neg_integer(), atom(), non_neg_integer()} | pull)
        -> reference() | undefined.
handle_data_retrival_method(TRef, {push, Interval, Mod, Amount}) ->
    cancel_timer(TRef),
    {ok, Ref} = timer:send_interval(Interval, {push_data, Mod, Amount}),
    Ref;
handle_data_retrival_method(TRef, pull) ->
    cancel_timer(TRef),
    undefined.

% terminate(Node, Cookie, TraceCLientPid) ->
terminate(#{ node := Node,
             cookie := Cookie,
             trace_client_pid := TraceCLientPid} = _State) ->
    try
        ok = disable_all_tracing(Node),
        true = goanna_db:delete_node(Node, Cookie),
        true = unlink(TraceCLientPid)
    catch
        C:E ->
            goanna_log:log("terminate TRY ~p ~p\n", [C, E])
    end,
    terminate.

%% -----------------------------------------------------------------------------------
%% DBG steps

trace_steps(Node, Cookie, tcpip_port) ->
    % goanna_log:log("going to call tcpip_port_trace_steps~n", []),
    tcpip_port_trace_steps(Node, Cookie).
% trace_steps(Node, Cookie, file) ->
%     file_port_trace_steps(Node, Cookie).
% trace_steps(Node, Cookie, erlang_distribution) ->
%     erlang_distribution_trace_steps(Node, Cookie).

-spec tcpip_port_trace_steps(atom(), atom())
        -> {ok, pid(), pid()} | {error, term()}.
tcpip_port_trace_steps(Node, Cookie) ->
    try
        [_Name, RelayHost] = string:tokens(atom_to_list(Node), "@"),
        case dbg_start(Node) of
            {ok, RemoteDbgPid} ->
                ok = wait_for_remote_code_server(Node),
                % Use port 0 so that the OS can assign a unused port
                PortGenerator = rpc:call(Node, dbg, trace_port, [ip, 0]),
                case rpc:call(Node, dbg, tracer, [port, PortGenerator]) of
                    {error, already_started} ->
                        ok;
                    {ok, RemoteDbgPid} ->
                        ok;
                    ERR ->
                        goanna_log:log("Starting tracer on remote node failed ~p\n\n", [ERR]),
                        ERR
                end,
                {ok,_MatchDesc} = dbg_p(Node),
                {ok, Fun} = handler_fun(goanna_node_sup:id(Node, Cookie), tcpip_port),
                {ok, TracePort} = rpc:call(Node, dbg, trace_port_control, [get_listen_port]),
                %goanna_log:log("TracePort ~p\n", [TracePort]),
                TraceCLientPid = dbg:trace_client(ip, {RelayHost, TracePort}, {Fun, ok}),
                true = link(TraceCLientPid),
                {ok, RemoteDbgPid, TraceCLientPid};
            Error ->
                goanna_log:log("tcpip_port_trace_steps error ~p~n", [Error]),
                Error
        end
    catch
        C:R ->
            goanna_log:log("was going to tcpip_port_trace_steps Encountered problem~n~p ~p~n",
                [C, R]),
            exit(self())
    end.

% -spec file_port_trace_steps(atom(), atom())
%         -> {ok, pid()} | {error, term()}.
% file_port_trace_steps(Node, Cookie) ->
%     case dbg_start(Node) of
%         {ok, RemoteDbgPid} ->
%             ok = wait_for_remote_code_server(Node),
%             PortGenerator = rpc:call(Node, dbg, trace_port, [file, "/Users/rp/hd2/code/goanna/tracefile"]),
%             case rpc:call(Node, dbg, tracer, [port, PortGenerator]) of
%                 {error, already_started} ->
%                     ok;
%                 {ok, RemoteDbgPid} ->
%                     ok
%             end,
%             {ok,_MatchDesc} = dbg_p(Node),
%             {ok, Fun} = handler_fun(Node, Cookie, file),
%             CLientPid = dbg:trace_client(file, "/Users/rp/hd2/code/goanna/tracefile", {Fun, ok}),
%             true = link(CLientPid),
%             goanna_log:log("RemoteDbgPid : ~p~n", [RemoteDbgPid]),
%             {ok, RemoteDbgPid};
%         Error ->
%             Error
%     end.

-spec dbg_start(atom()) -> {ok, pid()} | {error, term()}.
dbg_start(Node) ->
    try
        {ok, _Pid} = rpc:call(Node, dbg, start, [])
    catch
        error:{badmatch,{badrpc,nodedown}} ->
            {error, {badrpc,nodedown}};
        error:{badmatch,
                {badrpc,{'EXIT',{{case_clause,DbgPid},_}}}} ->
            {ok, DbgPid};
        C:E ->
            goanna_log:log("[~p] line ~p -> {~p, ~p}", [?MODULE, ?LINE, C, E]),
            {error, dbg_start_failed}
    end.

wait_for_remote_code_server(Node) ->
    wait_for_remote_code_server(Node, 40). %% 40 times, at 25ms a try = 1000ms

wait_for_remote_code_server(Node, Attempts) when Attempts =< 0 ->
    {error, {Node, undefined, code_server}};
wait_for_remote_code_server(Node, Attempts) when Attempts > 0 ->
    case rpc:call(Node, erlang, whereis, [code_server]) of
        undefined ->
            timer:sleep(25),
            wait_for_remote_code_server(Node, Attempts-1);
        Pid when is_pid(Pid) ->
            ok
    end.

dbg_p(Node) ->
    {ok,_MatchDesc} = rpc:call(Node, dbg, p, [all, [call, timestamp]]).

handler_fun(ChildId, tcpip_port) ->
    {ok, fun
        (Trace={trace_ts, _Pid, _Label, _Info, _ReportedTS}, _) ->
            ChildId ! {trace_item, Trace};
        (Trace={trace_ts, _Pid, _Label, _Info, _Extra, _ReportedTS}, _) ->
             ChildId ! {trace_item, Trace};
        (_Trace={drop, NumberOfDroppedItems}, _X) ->
            goanna_log:log("! Remote DBG dropped ~p message. Goanna could not consume fast enough!~n", [NumberOfDroppedItems]);
        (end_of_trace, _) ->
            ok;
        (_Trace, _X) ->
            ok
    end}.
%% TODO: implement file..
% handler_fun(_Node, _Cookie, file) ->
%     {ok, fun(_Trace, _) ->
%         % case goanna_api:recv_trace([trace, goanna_node_sup:id(Node,Cookie)],Trace) of
%         %     ok ->
%         %         ok;
%         %     stop_tracing ->
%         %         ok
%         % end
%         %%goanna_log:log("FILE TRACE: ~p\n", [Trace])
%         ok
%     end}.

trace(_ChildId, [], _) ->
    ok;
trace(ChildId, [H|T], Node) ->
    %% TODO: how to report, already traced...
    _ = trace(ChildId, H, Node),
    trace(ChildId, T, Node);
trace(ChildId, TrcPattern, Node) ->
    case goanna_db:lookup([tracelist, ChildId, TrcPattern]) of
        [] ->
            true = goanna_db:store_trace_pattern(ChildId, TrcPattern, []),
            {ok, _} = enable_tracing(Node, TrcPattern);
        [{{ChildId, TrcPattern}, _Opts}] ->
            {error, {TrcPattern, already_tracing}}
    end.

enable_tracing(Node, {M}) ->
    rpc:call(Node, dbg, tpl, [M, cx]); %% {ok, _MatchDesc}
enable_tracing(Node, {M, F}) ->
    rpc:call(Node, dbg, tpl, [M, F, cx]); %% {ok, _MatchDesc}
enable_tracing(Node, {M, F, A}) ->
    rpc:call(Node, dbg, tpl,[M, F, A, cx]); %% {ok, _MatchDesc}
enable_tracing(Node, {M, F, A, Ms}) ->
    rpc:call(Node, dbg, tpl,[M, F, A, Ms]). %% {ok, _MatchDesc}

-spec disable_all_tracing(atom()) -> ok.
disable_all_tracing(Node) ->
    remote_dbg_ctpl(Node, []),
    rpc:call(Node, dbg, stop_clear, []),
    ok.

clear_tracelist(Node, Cookie, Type) ->
    %% Just make sure dbg, and tracer is always started..
    %% TODO: actually, maybe only start DBG remotely when needed...( if tracing )
    [{Node, Cookie, Type}] = goanna_db:lookup([nodelist, Node]),
    true = goanna_db:truncate_tracelist([]).

% No "{ok,_} = " matching, becasue the node might have disconnected already.
remote_dbg_ctpl(Node, []) ->
    rpc:call(Node, dbg, ctpl, []);
remote_dbg_ctpl(Node, {Module}) ->
    rpc:call(Node, dbg, ctpl, [Module]);
remote_dbg_ctpl(Node, {Module, Function}) ->
    rpc:call(Node, dbg, ctpl, [Module, Function]);
remote_dbg_ctpl(Node, {Module, Function, Arity}) ->
    rpc:call(Node, dbg, ctpl, [Module, Function, Arity]).

cancel_timer(undefined) ->
    ok;
cancel_timer(TRef) ->
    try
        case timer:cancel(TRef) of
            {error,badarg} ->
                ok;
            {ok, cancel} ->
                ok
        end
    catch
        _C:_E ->
            ok
    end.

%% -----------------------------------------------------------------------------------
%% Sys calls

get_status(SysState, Parent, Mod, Debug, Misc) ->
    PDict = get(),
    FmtArgs = [PDict, SysState, Parent, Debug, Misc],
    FmtMisc = format_status(normal, FmtArgs),
    {status, self(), {module, Mod}, [PDict, SysState, Parent, Debug, FmtMisc]}.

format_status(_Opt, StatusData) ->
    [_PDict, SysState, Parent, _Debug, [Name, State, _Mod, _Time, _HibernateAfterTimeout]] = StatusData,
    Header = "Status for process "++atom_to_list(Name),
    Log = [],
    [{header, Header},
     {data, [{"Status", SysState},
             {"Parent", Parent},
             {"Logged events", Log},
             {"State", State}
            ]
    }].

debug(Fmt, Args) ->
    io:format(Fmt, Args).