-module(goanna_shell_printer).
-export([forward/2]).
-include_lib("goanna.hrl").
-behaviour(goanna_forward_callback_mod).


forward(Tbl, [{Now, TraceItem}]) ->
    %% Warning, to use the nice Yellow color, from the default config.
	?WARNING("[~p] [~p] ~p", [get_time(Now), Tbl, TraceItem]),
    ok.

get_time({_,_,Micro} = Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0sZ",
                  [Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).

