-module(goanna_forward_file_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CHILD_ID, ?MODULE).

instance_test_() ->
    {foreach,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [{spawn, [fun forward_init/0]},
      {spawn, [fun forward/0]}
     ]
    }.

forward_init() ->
    {ok, Pid} = goanna_forward_file:forward_init(?CHILD_ID),
    ?assert(
        is_pid(Pid)
    ).

forward() ->
    {ok, Pid} = goanna_forward_file:start_link(?CHILD_ID),
    ?assertEqual(
        ok,
        goanna_forward_file:forward(
            Pid,
            {{1517,250010,958719}, node, {trace_ts, self(), return_from, bla, {1517,250010,958719}}}
        )
    ),
    ?assert(
        is_pid(Pid)
    ),
    timer:sleep(100).

unit_test_() ->
    {foreach,
     fun() ->
        ok
     end,
     fun(_) ->
        Dir = goanna_forward_file:log_dir(),
        [ begin
            ok = file:delete(Dir++File)
          end || File <- goanna_forward_file:all_logs(Dir, ?CHILD_ID)
        ]
     end,
     [  fun forward_init/0,
        fun forward/0,
        fun filenumber/0,
        fun new_filename/0,
        fun maybe_rollover/0,
        fun next_new_filename/0,
        fun is_file_too_big/0,
        fun all_logs/0,
        fun file_open/0,
        fun new_file_open/0,
        fun do_log/0,
        fun log_dir/0,
        fun filename_from_path/0
     ]
    }.

filenumber() ->
    ?assertEqual(
        1,
        goanna_forward_file:filenumber("gff.goanna_forward_file_tests.1")
    ),
    ?assertEqual(
        999,
        goanna_forward_file:filenumber("gff.goanna_forward_file_tests.999")
    ).

new_filename() ->
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        goanna_forward_file:new_filename(?CHILD_ID, 0)
    ).

maybe_rollover() ->
    LogDir = goanna_forward_file:log_dir(),
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    {ok, FPID} = goanna_forward_file:file_open(LogDir++Filename),
    ?assertEqual(
        Filename,
        goanna_forward_file:filename_from_path(
            goanna_forward_file:maybe_rollover(?CHILD_ID, LogDir++Filename)
        )
    ),
    ok = goanna_forward_file:do_log(FPID, binary_to_list(<<1:(8*2000*1000)>>)),
    timer:sleep(100),
    ?assertEqual(
        "gff.goanna_forward_file_tests.2",
        goanna_forward_file:maybe_rollover(?CHILD_ID, LogDir++Filename)
    ).

next_new_filename() ->
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    ?assertEqual(
        "gff.goanna_forward_file_tests.2",
        goanna_forward_file:next_new_filename(?CHILD_ID, Filename)
    ).

is_file_too_big() ->
    LogDir = goanna_forward_file:log_dir(),
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    {ok, FPID} = goanna_forward_file:file_open(LogDir++Filename),
    ?assertEqual(
        false,
        goanna_forward_file:is_file_too_big(LogDir++Filename)
    ),
    ok = goanna_forward_file:do_log(FPID, binary_to_list(<<1:(8*2000*1000)>>)),
    timer:sleep(100),
    ?assertEqual(
        true,
        goanna_forward_file:is_file_too_big(LogDir++Filename)
    ).

all_logs() ->
    LogDir = goanna_forward_file:log_dir(),
    ?assertEqual(
        [],
        goanna_forward_file:all_logs(LogDir, ?CHILD_ID)
    ),
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    {ok, _FPID} = goanna_forward_file:file_open(LogDir++Filename),
    ?assertEqual(
        [Filename],
        goanna_forward_file:all_logs(LogDir, ?CHILD_ID)
    ).

file_open() ->
    LogDir = goanna_forward_file:log_dir(),
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    {ok, FPID} = goanna_forward_file:file_open(LogDir++Filename),
    ?assertEqual(
        ok,
        file:write(FPID, <<1>>)
    ).

new_file_open() ->
    LogDir = goanna_forward_file:log_dir(),
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    NewFilename = goanna_forward_file:new_filename(?CHILD_ID, 1),
    {ok, FPID} = goanna_forward_file:file_open(LogDir++Filename),
    {ok, NewFPID} = goanna_forward_file:new_file_open(FPID, LogDir++NewFilename),
    ?assertEqual(
        {error,einval},
        file:write(FPID, <<1>>)
    ),
    ?assertEqual(
        ok,
        file:write(NewFPID, <<1>>)
    ).

do_log() ->
    LogDir = goanna_forward_file:log_dir(),
    Filename = goanna_forward_file:new_filename(?CHILD_ID, 0),
    ?assertEqual(
        "gff.goanna_forward_file_tests.1",
        Filename
    ),
    {ok, FPID} = goanna_forward_file:file_open(LogDir++Filename),
    ?assertEqual(
        ok,
        goanna_forward_file:do_log(FPID, "list")
    ).

log_dir() ->
    {ok, CWD} = file:get_cwd(),
    ?assertEqual(
        CWD++"/log/",
        goanna_forward_file:log_dir()
    ).

filename_from_path() ->
    ?assertEqual(
        "file",
        goanna_forward_file:filename_from_path("/tmp/dir1/dir2/file")
    ).