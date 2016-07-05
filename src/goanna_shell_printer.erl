-module(goanna_shell_printer).
-export([forward/2]).
-include_lib("goanna.hrl").


forward(Tbl, [{Now, TraceItem}]) ->
	?INFO("[~p] [~p] ~p", [get_time(Now), Tbl, TraceItem]).

get_time({_,_,Micro} = Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0sZ",
                  [Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).

