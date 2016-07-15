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