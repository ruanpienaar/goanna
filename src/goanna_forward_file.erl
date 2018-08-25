-module(goanna_forward_file).

-behaviour(goanna_forward_callback_mod).
-export([
    forward_init/1,
    forward/2
]).

-ifdef(TEST).
-export([
    start_link/1,
    filenumber/1,
    new_filename/2,
    maybe_rollover/2,
    next_new_filename/2,
    is_file_too_big/1,
    all_logs/2,
    file_open/1,
    new_file_open/2,
    do_log/2,
    log_dir/0,
    filename_from_path/1
]).
-endif.

-define(MAX_FILE_SIZE, 1024.00).
-define(MAX_ENTRIES, 12000).

%% @doc
%%   For now, it only produces 1 file.
%%   This produces gff.1, gff.2, etc etc files, with trace items inside.
%% @end

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(STATE, goanna_forward_file).
-record(?STATE, {
    child_id,
    filename,
    fpid,
    entries = 0, %% Rolver at X trace entries.
    size = 0     %% Rollover at X Kbytes
}).

%% ----------

-spec forward_init(atom()) -> {ok, pid() | atom()} | {error, term()}.
forward_init(ChildId) ->
    case start_link(ChildId) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec forward(Process :: pid() | atom(), goanna_forward_callback_mod:goanna_trace_tuple()) -> ok.
forward(Process, {_, Node, TraceItem}) ->
    gen_server:cast(Process, {entry, Node, TraceItem}).

start_link(ChildId) ->
    gen_server:start_link(?MODULE, {ChildId}, []).

%% ----------

init({ChildId}) ->
    LD = log_dir(),
    true = filelib:is_dir(LD),
    CurrentLogFile =
        case all_logs(LD, ChildId) of
            [] ->
                new_filename(ChildId, 0);
            [LastGFFFile|_] ->
                maybe_rollover(ChildId, LD++LastGFFFile)
        end,
    {ok, FPID} = file_open(LD++CurrentLogFile),
    {ok, #?STATE{
        child_id = ChildId,
        filename = CurrentLogFile,
        fpid = FPID
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({entry, Node, TraceItem}, #?STATE{ fpid = FPID,
                                               filename = Filename,
                                               entries = E,
                                               child_id = ChildId } = State) when E > ?MAX_ENTRIES ->
    LD = log_dir(),
    TraceString = goanna_common:format_trace_item(Node, TraceItem),
    NewFileName = next_new_filename(ChildId, LD++Filename),
    {ok, NewFPID} = new_file_open(FPID, LD++NewFileName),
    ok = do_log(NewFPID, TraceString),
    {noreply, State#?STATE{ fpid = NewFPID, filename = NewFileName }};
handle_cast({entry, Node, TraceItem}, #?STATE{ fpid = FPID,
                                               filename = Filename,
                                               child_id = ChildId } = State) ->
    LD = log_dir(),
    TraceString = goanna_common:format_trace_item(Node, TraceItem),
    ok = do_log(FPID, TraceString),
    case maybe_rollover(ChildId, LD++Filename) of
        Filename ->
            {noreply, State};
        NewFilename ->
            {ok, NewFPID} = new_file_open(FPID, LD++NewFilename),
            {noreply, State#?STATE{ fpid = NewFPID, filename = NewFilename }}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #?STATE{ fpid = FPID } = _State) ->
    file:close(FPID).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------

filenumber([$g,$f,$f,$.|Rest]) ->
    [_ChildId, R] = string:tokens(Rest, "."),
    list_to_integer(R).

new_filename(ChildId, Filecount) ->
    "gff."++atom_to_list(ChildId)++"."++integer_to_list(Filecount+1).

%% gives new filename
maybe_rollover(ChildId, Filename) ->
    case is_file_too_big(Filename) of
        true ->
            next_new_filename(ChildId, Filename);
        false ->
            filename_from_path(Filename)
    end.

next_new_filename(ChildId, Filename) ->
    new_filename(ChildId, filenumber(filename_from_path(Filename))).

is_file_too_big(Filename) ->
    filelib:file_size(Filename) / 1000 > ?MAX_FILE_SIZE.

all_logs(Dir, ChildId) ->
    filelib:fold_files(Dir, "gff."++atom_to_list(ChildId)++".*", false, fun(Filename, AccIn) ->
        [filename_from_path(Filename)|AccIn]
    end, []).

file_open(Filename) ->
    % TODO: {delayed_write, 65536, 500} is crashing the unit test in 21.0
    file:open(Filename, [append, write, raw]).

new_file_open(FPID, Filename) ->
    ok = file:close(FPID),
    file_open(Filename).

do_log(FPID, TraceString) ->
    ok = file:write(FPID, list_to_binary(TraceString)).

% Change into a value , places in state.
log_dir() ->
    {ok,Dir} = file:get_cwd(),
    Dir++"/log/".

filename_from_path(Path) ->
    lists:last(string:tokens(Path, "/")).

