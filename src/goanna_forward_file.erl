-module(goanna_forward_file).

-behaviour(goanna_forward_callback_mod).
-export([
    forward_init/1,
    forward/2
]).

-define(MAX_FILE_SIZE, 1024.00).
-define(MAX_ENTRIES, 12000).

%% @doc
%%   For now, it only produces 1 file.
%%   This produces gff.1, gff.2, etc etc files, with trace items inside.
%% @end

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(STATE, goanna_forward_file).
-record(?STATE, {
    filename,
    fpid,
    entries = 0, %% Rolver at 1000 000 entries.
    size = 0     %% Rollover at 1096 Kbyte
}).

-spec forward_init(X :: any()) -> ok.
forward_init(_X) ->
     %% Let the first node to get here, start it, and let the other nodes use the same PID.
    case start_link() of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok
    end.

-spec forward(Tbl :: term(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(Tbl, {_, TraceItem}) ->
    String = goanna_forward_shell:format_trace_item(Tbl, TraceItem),
    gen_server:cast(?MODULE, {entry, String}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    CurrentLogFile =
        case all_logs(log_dir()) of
            [] ->
                new_filename(0);
            [LastGFFFile|_] ->
                maybe_rollover(LastGFFFile) %% XXX
        end,
    {ok, FPID} = file_open(CurrentLogFile),
    {ok, #?STATE{ filename = CurrentLogFile, fpid = FPID }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({entry, TraceEntry}, #?STATE{ fpid = FPID,
                                          filename = Filename,
                                          entries = E } = State) when E > ?MAX_ENTRIES ->
    NewFileName = next_new_filename(Filename), %% XXX
    {ok, NewFPID} = file_open(FPID, NewFileName),
    ok = do_log(NewFPID, TraceEntry),
    {noreply, State#?STATE{ fpid = NewFPID, filename = NewFileName }};
handle_cast({entry, TraceEntry}, #?STATE{ fpid = FPID,
                                          filename = Filename } = State) ->
    ok = do_log(FPID, TraceEntry),
    case maybe_rollover(Filename) of %% XXX
        Filename ->
            {noreply, State};
        NewFilename ->
            {ok, NewFPID} = file_open(FPID, NewFilename),
            {noreply, State#?STATE{ fpid = NewFPID, filename = NewFilename }}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #?STATE{ fpid = FPID } = _State) ->
    ok = file:close(FPID),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

filenumber([$g,$f,$f,$.|R]) ->
    list_to_integer(R).

new_filename(Filecount) ->
    "gff."++integer_to_list(Filecount).

%% gives new filename
maybe_rollover(Filename) ->
    case is_file_too_big(Filename) of
        true ->
            next_new_filename(Filename);
        false ->
            Filename
    end.

next_new_filename(Filename) ->
    new_filename(filenumber(Filename)+1).

is_file_too_big(Filename) ->
    filelib:file_size(log_dir()++Filename) / 1000 > ?MAX_FILE_SIZE.

all_logs(Dir) ->
    filelib:fold_files(Dir, "gff.*", false, fun(Filename, AccIn) ->
        [filename_from_path(Filename)|AccIn]
    end, []).

file_open(Filename) ->
    file:open(log_dir()++Filename, [append, write, raw, {delayed_write, 65536, 500}]).

file_open(FPID, Filename) ->
    ok = file:close(FPID),
    file_open(Filename).

do_log(FPID, TraceEntry) ->
    ok = file:write(FPID, list_to_binary(TraceEntry)).

log_dir() ->
    {ok,Dir} = file:get_cwd(),
    Dir++"/log/".

filename_from_path(Path) ->
    lists:last(string:tokens(Path, "/")).