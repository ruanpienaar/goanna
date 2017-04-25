-module(goanna_os_io_printer).

-behaviour(goanna_forward_callback_mod).

-export([forward/2]).

-export([% log_levels/0,
         trace_abbreviations/0
]).

-include_lib("goanna.hrl").

-define(FILENAME, 
    code:lib_dir(goanna)++"/log/os_io_printer_traces").

-spec forward(Tbl :: term(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
%% Warning is used for the nice Yellow color, from the default lager config.
forward(Tbl, {_, TraceItem}) ->
    log_trace_entry(Tbl,TraceItem).

get_time({_,_,Micro} = Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0sZ",
                  [Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).

-spec log_trace_entry(atom(), goanna_forward_callback_mod:erlang_trace_data()) -> string().

%% trace_ts
log_trace_entry(T,_Trace={trace_ts, _Pid, exception_from, Info, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p", [get_time(ReportedTS), T, get_trace_abbreviation(exception_from), Info]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, return_from, Info, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p", [get_time(ReportedTS), T, get_trace_abbreviation(return_from), Info]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, call, Info, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p", [get_time(ReportedTS), T, get_trace_abbreviation(call), Info]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, Label, Info, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p", [get_time(ReportedTS), T, get_trace_abbreviation(Label), Info]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, exception_from, Info, Extra, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p ~p", [get_time(ReportedTS), T, get_trace_abbreviation(exception_from), Info, Extra]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, return_from, Info, Extra, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p ~p", [get_time(ReportedTS), T, get_trace_abbreviation(return_from), Info, Extra]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, call, Info, Extra, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p ~p", [get_time(ReportedTS), T, get_trace_abbreviation(call), Info, Extra]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,_Trace={trace_ts, _Pid, Label, Info, Extra, ReportedTS}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~s ~p ~.5s: ~p ~p", [get_time(ReportedTS), T, get_trace_abbreviation(Label), Info, Extra]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,Trace={seq_trace, _Label, _SeqTraceInfo}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~p ~p", [T, Trace]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,Trace={drop, _NumberOfDroppedItems}) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~p ~p", [T, Trace]),
        ?FILENAME]))),
    ok;
log_trace_entry(T,Trace) ->
    os:cmd(lists:flatten(io_lib:format("echo \"~s\" >> ~s",
        [io_lib:format("~p ~p", [T, Trace]),
        ?FILENAME]))),
    ok.

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

-spec trace_abbreviations() -> ok.
trace_abbreviations() ->
    io:format("receive                      REC~n", []),
    io:format("send                         S~n", []),
    io:format("send_to_non_existing_process STNEP~n", []),
    io:format("call                         C~n", []),
    io:format("return_to                    RT~n", []),
    io:format("return_from                  RF~n", []),
    io:format("exception_from               EF~n", []),
    io:format("spawn                        SPW~n", []),
    io:format("exit                         EXI~n", []),
    io:format("link                         LI~n", []),
    io:format("unlink                       ULI~n", []),
    io:format("getting_linked               GLI~n", []),
    io:format("getting_unlinked             GULI~n", []),
    io:format("register                     REG~n", []),
    io:format("unregister                   UNREG~n", []),
    io:format("in                           I~n", []),
    io:format("out                          O~n", []),
    io:format("gc_start                     GCS~n", []),
    io:format("gc_end                       GCE~n", []).
