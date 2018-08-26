-module(goanna_log).

-export([log/2]).

% Use some logging tool later,
% i used to use lager, but that was too much
% for just simple logging

log(Fmt, Args) ->
    case application:get_env(goanna, log_level, normal) of
        normal ->
            ok;
        debug ->
            io:format(Fmt, Args)
    end.