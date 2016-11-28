-module(goanna_forward_callback_mod).

%% Have a look at et.
%% Also at et_selector.erl & et_collector.erl

% Label intended to provide a brief event summary.
-type event_summary_label() :: 
    'receive' |
    send |
    send_to_non_existing_process |
    call |
    return_to |
    return_from |
    exception_from |
    spawn |
    exit |
    link |
    unlink |
    getting_linked |
    getting_unlinked |
    register |
    unregister |
    in |
    out |
    gc_start |
    gc_end.

% The label component is an integer which identifies all events belonging to the same sequential trace. If several sequential traces can be active simultaneously, label is used to identify the separate traces. Default is 0.
-type seq_trace_label() :: integer().

% this is usually the meat of the trace mesage:
% {module, function, arity} | 
-type msg() :: any().

% This is usually the value, of type event_summary_label() 'return_[to/from]'
% Ex: {trace_ts,<3099.572.0>,return_from,{ets,info,2},0,{1468,331075,614047}}
% 0 was the the return value
-type return_val() :: any().

%% TODO: for later, flesh out the exact values:
-type seq_trace_info() :: 
    % Used when a process From with its trace token flag print set to true has sent a message.
    {send, Serial :: any(), From :: any(), To :: any(), Message :: any()} |
    % Used when a process To receives a message with a trace token that has flag 'receive' set to true.
    {'receive', Serial :: any(), From :: any(), To :: any(), Message :: any()} | 
    % Used when a process From has called seq_trace:print(Label, TraceInfo) and has a trace token with flag print set to true, and label set to Label.
    {print, Serial :: any(), From :: any(), _ :: any(), Info :: any()}.

-type erlang_trace_data() ::
    {trace, Pid :: pid(), Label :: event_summary_label(), Info :: msg()} |
    {trace, Pid :: pid(), Label :: event_summary_label(), Info :: msg(), Extra :: return_val()} |
    {trace_ts, Pid :: pid(), Label :: event_summary_label(), Info :: msg(), ReportedTS :: calendar:timestamp()} |
    {trace_ts, Pid :: pid(), Label :: event_summary_label(), Info :: msg(), Extra :: return_val(), ReportedTS :: calendar:timestamp()} |
    {seq_trace, Label :: seq_trace_label(), SeqTraceInfo :: seq_trace_info()} |
    {seq_trace, Label :: seq_trace_label(), SeqTraceInfo :: seq_trace_info(), ReportedTS :: calendar:timestamp()} |
    {drop, NumberOfDroppedItems :: non_neg_integer()}.

-type goanna_trace_tuple() :: {calendar:timestamp(), erlang_trace_data()}.

-callback forward(Node :: node(), Item :: goanna_trace_tuple()) -> ok.


-export_type([goanna_trace_tuple/0]).
-export_type([erlang_trace_data/0]).