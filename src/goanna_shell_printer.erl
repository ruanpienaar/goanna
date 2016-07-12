-module(goanna_shell_printer).

-behaviour(goanna_forward_callback_mod).

-export([forward/2]).

-include_lib("goanna.hrl").

-spec forward(Tbl :: term(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
%% Warning is used for the nice Yellow color, from the default lager config.
forward(Tbl, {Now, TraceItem}) ->
	?WARNING("[~p] [~p] ~p", [get_time(Now), Tbl, format_trace_item(TraceItem)]),
    ok.

get_time({_,_,Micro} = Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0sZ",
                  [Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).

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
