-define(DEBUG(Msg),io:format(Msg++"~n")).
-define(DEBUG(Msg, Args),io:format(Msg++"~n", Args)).

-define(INFO(Msg),io:format(Msg++"~n")).
-define(INFO(Msg, Args),io:format(Msg++"~n", Args)).

-define(NOTICE(Msg),io:format(Msg++"~n")).
-define(NOTICE(Msg, Args),io:format(Msg++"~n", Args)).

-define(WARNING(Msg),io:format(Msg++"~n")).
-define(WARNING(Msg, Args),io:format(Msg++"~n", Args)).

-define(ERROR(Msg),io:format(Msg++"~n")).
-define(ERROR(Msg, Args),io:format(Msg++"~n", Args)).

-define(CRITICAL(Msg),io:format(Msg++"~n")).
-define(CRITICAL(Msg, Args),io:format(Msg++"~n", Args)).

-define(ALERT(Msg),io:format(Msg++"~n")).
-define(ALERT(Msg, Args),io:format(Msg++"~n", Args)).

-define(EMERGENCY(Msg),io:format(Msg++"~n")).
-define(EMERGENCY(Msg, Args),io:format(Msg++"~n", Args)).

-record(trc_pattern,{
    m,
    f,
    a,
    ms
}).

-define(GOANNA_STATE, goanna_state).
-record(?GOANNA_STATE,
    {node, cookie, type, child_id,
     connected=false, connect_attempt_ref=undefined, connect_attempts=0, max_reconnecion_attempts=0,
     data_retrival_method=pull :: {push, non_neg_integer(), atom()} | pull, push_pending,
     trace_msg_count=0, trace_msg_total, trace_time, trace_timer_tref=false,
     trace_active=false
    }).

-define(NODE_COOKIE_SEP, "©").