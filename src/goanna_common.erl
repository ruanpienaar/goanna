-module(goanna_common).
-export([
    prop_value/3
]).

-spec prop_value(term(), proplists:proplist(), term()) -> term().
prop_value(Field, Opts, Default) ->
    case lists:keyfind(Field, 1, Opts) of
        false          -> Default;
        {Field, Value} -> Value
    end.