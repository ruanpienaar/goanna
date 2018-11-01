-module(goanna_common).
-export([
    prop_value/3,
    get_trace_abbreviation/1,
    trace_abbreviations/0,
    format_trace_item/2
]).

-ifdef(TEST).
-export([
    get_time/1
]).
-endif.

-spec prop_value(term(), proplists:proplist(), term()) -> term().
prop_value(Field, Opts, Default) ->
    case lists:keyfind(Field, 1, Opts) of
        false          -> Default;
        {Field, Value} -> Value
    end.

% -spec log_levels() -> ok.
% log_levels() ->
%     ?DEBUG("DEBUG", []),
%     ?INFO("INFO", []),
%     ?NOTICE("NOTICE", []),
%     ?WARNING("WARNING", []),
%     ?ERROR("ERROR", []),
%     ?CRITICAL("CRITICAL", []),
%     ?ALERT("ALERT", []),
%     ?EMERGENCY("EMERGENCY", []).

get_trace_abbreviation('receive') ->
    "REC";
get_trace_abbreviation(send) ->
    "S";
get_trace_abbreviation(send_to_non_existing_process) ->
    "STNEP";
get_trace_abbreviation(call) ->
    "C";
get_trace_abbreviation(return_to) ->
    "RT";
get_trace_abbreviation(return_from) ->
    "RF";
get_trace_abbreviation(exception_from) ->
    "EF";
get_trace_abbreviation(spawn) ->
    "SPW";
get_trace_abbreviation(exit) ->
    "EXI";
get_trace_abbreviation(link) ->
    "LI";
get_trace_abbreviation(unlink) ->
    "ULI";
get_trace_abbreviation(getting_linked) ->
    "GLI";
get_trace_abbreviation(getting_unlinked) ->
    "GULI";
get_trace_abbreviation(register) ->
    "REG";
get_trace_abbreviation(unregister) ->
    "UNREG";
get_trace_abbreviation(in) ->
    "I";
get_trace_abbreviation(out) ->
    "O";
get_trace_abbreviation(gc_start) ->
    "GCS";
get_trace_abbreviation(gc_end) ->
    "GCE".

trace_abbreviations() ->
    L = ['receive', send, send_to_non_existing_process, call, return_to, 
         return_from, exception_from, spawn, exit, link, unlink, 
         getting_linked, getting_unlinked, register, unregister, 
         in, out, gc_start, gc_end],
    lists:foreach(fun(I) -> io:format("~p\t~p\n", [I, get_trace_abbreviation(I)]) end, L).

-spec format_trace_item(atom(), goanna_forward_callback_mod:erlang_trace_data()) -> string().
format_trace_item(Node, _Trace={trace_ts, _Pid, exception_from, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(exception_from), Info]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, return_from, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(return_from), Info]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, call, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(call), Info]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, Label, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(Label), Info]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, exception_from, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(exception_from), Info, Extra]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, return_from, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(return_from), Info, Extra]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, call, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(call), Info, Extra]
    );
format_trace_item(Node, _Trace={trace_ts, _Pid, Label, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~.5s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(Label), Info, Extra]
    );
format_trace_item(Node, Trace={seq_trace, _Label, _SeqTraceInfo}) ->
   io_lib:format(
        "~p ~p", 
        [Node, Trace]
    );
format_trace_item(Node, Trace={drop, _NumberOfDroppedItems}) ->
    io_lib:format(
        "~p ~p", 
        [Node, Trace]
    );
format_trace_item(Node, Trace) ->
    io_lib:format(
        "~p ~p", 
        [Node, Trace]
    ).

get_time({_,_,Micro} = Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~p",
                  [Year, Month, Day, Hour, Minute, Second, Micro])).