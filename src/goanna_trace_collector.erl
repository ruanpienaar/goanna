-module(goanna_trace_collector).

-export([start_link/0]).

-include_lib("goanna.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, goanna_trace_collector).
-record(?STATE, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    {ok, #?STATE{}}.

handle_call(Request, _From, State) ->
    ?EMERGENCY("[~p] Unknown handle_call ~p~n~p", [?MODULE, Request, State]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    ?EMERGENCY("[~p] Unknown handle_cast ~p~n~p", [?MODULE, Msg, State]),
    {noreply, State}.

handle_info({trace_item, NodeChildId, Trace}, State) ->
    ok = gen_server:call(NodeChildId, {trace_item, Trace}, infinity),
    {noreply, State};
handle_info(Info, State) ->
    ?EMERGENCY("[~p] Unknown handle_info ~p~n~p", [?MODULE, Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.