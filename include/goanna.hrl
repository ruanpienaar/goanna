-define(DEBUG(Msg),lager:debug(Msg)).
-define(DEBUG(Msg, Args),lager:debug(Msg, Args)).

-define(INFO(Msg),lager:info(Msg)).
-define(INFO(Msg, Args),lager:info(Msg, Args)).

-define(NOTICE(Msg),lager:notice(Msg)).
-define(NOTICE(Msg, Args),lager:notice(Msg, Args)).

-define(WARNING(Msg),lager:warning(Msg)).
-define(WARNING(Msg, Args),lager:warning(Msg, Args)).

-define(ERROR(Msg),lager:error(Msg)).
-define(ERROR(Msg, Args),lager:error(Msg, Args)).

-define(CRITICAL(Msg),lager:critical(Msg)).
-define(CRITICAL(Msg, Args),lager:critical(Msg, Args)).

-define(ALERT(Msg),lager:alert(Msg)).
-define(ALERT(Msg, Args),lager:alert(Msg, Args)).

-define(EMERGENCY(Msg),lager:emergency(Msg)).
-define(EMERGENCY(Msg, Args),lager:emergency(Msg, Args)).

-record(trc_pattern,{
    m,
    f,
    a
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