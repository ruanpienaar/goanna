-module(_tests).
%%-author('').

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").

-define(MODNAME, ).
%%%.
%%%' TEST GENERATOR
%% @spec _test_() -> List
%% where
%%       List = [term()]
_test_() ->
  %% add your asserts in the returned list, e.g.:
  %% [
  %%   ?assert(?MODNAME:double(2) =:= 4),
  %%   ?assertMatch({ok, Pid}, ?MODNAME:spawn_link()),
  %%   ?assertEqual("ba", ?MODNAME:reverse("ab")),
  %%   ?assertError(badarith, ?MODNAME:divide(X, 0)),
  %%   ?assertExit(normal, ?MODNAME:exit(normal)),
  %%   ?assertThrow({not_found, _}, ?MODNAME:func(unknown_object))
  %% ]
  [].
%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:

