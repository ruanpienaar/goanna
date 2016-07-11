-module(goanna_forward_callback_mod).

% -callback init(Args :: list(term())) -> 'ok'|tuple('error', Reason :: string()).

% -callback handle(Event :: atom()) -> NextEvent :: atom().

% -callback sync(Node :: node(), Timeout :: non_neg_integer()) -> 'ok'|tuple('error', Reason :: string()).

-callback forward(Node :: node(), Item :: nonempty_list( tuple() )) -> ok.