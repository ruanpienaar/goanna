-module(goanna_trace_logger).

-behaviour(goanna_forward_callback_mod).
-behaviour(gen_event).

-export([forward/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------
%% API

-spec forward(Tbl :: term(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(_Tbl, {_, _TraceItem}) ->
    % format_trace_item(Tbl,TraceItem).
    ok.

%%------------------------------------------------------------------------

init(_A) ->
    %io:format("inti:~p~n", [A]),
    {ok, undefined}.

handle_event(_Event, State) ->
    %io:format("Event:~p~n", [Event]),
    {ok, State}.

handle_call(_Request, _State) ->
    %io:format("Request:~p~n", [Request]),
    {remove_handler, {error, unknown_call}}.

handle_info(_Info, State) ->
    %io:format("Info:~p~n", [Info]),
    {ok, State}.

terminate(_Reason, _State) ->
    %io:format("Reason:~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------