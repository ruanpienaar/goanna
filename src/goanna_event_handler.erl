-module(goanna_event_handler).

-behaviour(goanna_forward_callback_mod).
-behaviour(gen_event).

-export([forward/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-spec forward(Tbl :: term(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(Tbl, {_, TraceItem}) ->
    String=goanna_shell_printer:format_trace_item(Tbl,TraceItem),
    log_str(String).

init([LogDir, LogFileSize, LogFileMax]) ->
    io:format("[~p] init:~p~n", [?MODULE, [LogDir, LogFileSize, LogFileMax]]),
    {ok, undefined}.

%% Generated when error_msg/1,2 or format is called.
handle_event(E={error, _Gleader, {_Pid, _Format, _Data}}, State) ->
    log(E),
    {ok, State};
%% Generated when error_report/1 is called.
handle_event(E={error_report, _Gleader, {_Pid, std_error, _Report}}, State) ->
    log(E),
    {ok, State};
%% Generated when error_report/2 is called.
handle_event(E={error_report, _Gleader, {_Pid, _Type, _Report}}, State) ->
    log(E),
    {ok, State};
%% Generated when warning_msg/1,2 is called if warnings are set to be tagged as warnings.
handle_event(E={warning_msg, _Gleader, {_Pid, _Format, _Data}}, State) ->
    log(E),
    {ok, State};
%% Generated when warning_report/1 is called if warnings are set to be tagged as warnings.
handle_event(E={warning_report, _Gleader, {_Pid, std_warning, _Report}}, State) ->
    log(E),
    {ok, State};
%% Generated when warning_report/2 is called if warnings are set to be tagged as warnings.
handle_event(E={warning_report, _Gleader, {_Pid, _Type, _Report}}, State) ->
    log(E),
    {ok, State};
%% Generated when info_msg/1,2 is called.
handle_event(E={info_msg, _Gleader, {_Pid, _Format, _Data}}, State) ->
    log(E),
    {ok, State};
%% Generated when info_report/1 is called.
handle_event(E={info_report, _Gleader, {_Pid, std_info, _Report}}, State) ->
    log(E),
    {ok, State};
%% Generated when info_report/2 is called.
handle_event(E={info_report, _Gleader, {_Pid, _Type, _Report}}, State) ->
    log(E),
    {ok, State};
handle_event(E, State) ->
    log(E),
    {ok, State}.

handle_call(Request, State) ->
    io:format("[~p] handle_call:~p~n", [?MODULE, Request]),
    % {remove_handler, {error, unknown_call}}.
    {ok, State}.

handle_info(Info, State) ->
    io:format("[~p] handle_info:~p~n", [?MODULE, Info]),
    {ok, State}.

terminate(Reason, _State) ->
    io:format("[~p] terminate:~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log(E) ->
    io:format("~p~n", [E]).

log_str(Str) ->
    io:format("~s", [Str]).