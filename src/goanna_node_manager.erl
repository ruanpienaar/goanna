-module(goanna_node_manager).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    {ok, undefined}.

%% TODO: maybe record all children...
handle_call({start_child, Node, Cookie, Type}, _From, State) ->
    {reply, goanna_node_sup:start_child(Node, Cookie, Type), State};
handle_call({start_child_sync, Node, Cookie, Type}, _From, State) ->
    {reply, goanna_node_sup:start_child_sync(Node, Cookie, Type), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.