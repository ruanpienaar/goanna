-module(goanna_tracer).
-export([
    start_link/0,
    tracer/0
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, goanna_tracer_state).
-record(?STATE, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    tracer(),
    {ok, #?STATE{}}.

tracer() ->
    ok = dbg:stop_clear(),
    case whereis(dbg) of
        undefined ->
            {ok,P} = dbg:start(),
            link(P);
        P ->
            link(P),
            ok
    end,
    TraceFn = fun (Trace, _) ->
        gen_server:cast(?MODULE, Trace)
    end,
    {ok, TracerPid} = dbg:tracer(process, {TraceFn, ok}),
    io:format("TracerPid : ~p\n", [TracerPid]),
    % link(TracerPid),
    ok.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%%     {trace, Pid, Label, Info} |
%%     {trace, Pid, Label, Info, Extra} |
%%     {trace_ts, Pid, Label, Info, ReportedTS} |
%%     {trace_ts, Pid, Label, Info, Extra, ReportedTS} |
%%     {seq_trace, Label, Info} |
%%     {seq_trace, Label, Info, ReportedTS} |
%%     {drop, NumberOfDroppedItems}
handle_cast(TrcMsg = {trace, _Pid, _Label, _Info}, State) ->
    handle_trace_message(TrcMsg),
    {noreply, State};
handle_cast(TrcMsg = {trace, _Pid, _Label, _Info, _Extra}, State) ->
    handle_trace_message(TrcMsg),
    {noreply, State};
handle_cast(TrcMsg = {trace_ts, _Pid, _Label, _Info, _ReportedTS}, State) ->
    io:format("no implementation yet...~n~p~n", [TrcMsg]),
    {noreply, State};
handle_cast(TrcMsg = {trace_ts, _Pid, _Label, _Info, _Extra, _ReportedTS}, State) ->
    io:format("no implementation yet...~n~p~n", [TrcMsg]),
    {noreply, State};
handle_cast(TrcMsg = {seq_trace, _Label, _Info}, State) ->
    io:format("no implementation yet...~n~p~n", [TrcMsg]),
    {noreply, State};
handle_cast(TrcMsg = {seq_trace, _Label, _Info, _ReportedTS}, State) ->
    io:format("no implementation yet...~n~p~n", [TrcMsg]),
    {noreply, State};
handle_cast(TrcMsg = {drop, _NumberOfDroppedItems}, State) ->
    io:format("no implementation yet...~n~p~n", [TrcMsg]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("!!! UNKNOWN CAST~n~p~n\n", [Msg]),
    {noreply, State}.

%% --------
handle_trace_message(TrcMsg) ->
    Pid = element(2, TrcMsg),
    Node = node(Pid),
    % io:format("Trace for node: ~p~n", [Node]),
    case ets:lookup(nodelist, Node) of
        [{Node, Cookie}] ->
            % io:format("Node: ~p, Cookie~p~n~p~n",
            %     [Node, Cookie, TrcMsg]),
            true = goanna_db:store([trace, Node, Cookie], TrcMsg);
        [] ->
            io:format("No Node!~n", [])
    end.
%% --------

handle_info(Info, State) ->
    io:format("Info : ~p\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------