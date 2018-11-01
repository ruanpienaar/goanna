-module(goanna_common_tests).
-include_lib("eunit/include/eunit.hrl").

goanna_common_unit_test_() ->
    {setup,
     % Setup Fixture
     fun() -> 
         xxx
     end,
     % Cleanup Fixture
     fun(xxx) ->
         ok
     end,
     % List of tests
     [
       % Example test
        {"goanna_common:prop_value/0",
            ?_assert(unit_testing:try_test_fun(fun prop_value/0))},
        {"goanna_common:get_trace_abbreviation/1",
            ?_assert(unit_testing:try_test_fun(fun get_trace_abbreviation/0))},
        {"goanna_common:trace_abbreviations/0",
            ?_assert(unit_testing:try_test_fun(fun trace_abbreviations/0))},
        {"goanna_common:format_trace_item/0",
            ?_assert(unit_testing:try_test_fun(fun format_trace_item/0))},
        {"goanna_common:get_time/0",
            ?_assert(unit_testing:try_test_fun(fun get_time/0))}
     ]
    }.

prop_value() ->
    ?assertEqual(
        default,
        goanna_common:prop_value(field, [], default)
    ),
    ?assertEqual(
        value,
        goanna_common:prop_value(field, [{field, value}], default)
    ),
    ?assertEqual(
        other,
        goanna_common:prop_value(field, [{field, other}, {field, value}], default)
    ).

get_trace_abbreviation() ->
    ?assertEqual(
        "REC",
        goanna_common:get_trace_abbreviation('receive')
    ),        
    ?assertEqual(
        "S",    
        goanna_common:get_trace_abbreviation(send)
    ),    
    ?assertEqual(
        "STNEP",    
        goanna_common:get_trace_abbreviation(send_to_non_existing_process)
    ),    
    ?assertEqual(
        "C",    
        goanna_common:get_trace_abbreviation(call)
    ),    
    ?assertEqual(
        "RT",    
        goanna_common:get_trace_abbreviation(return_to)
    ),    
    ?assertEqual(
        "RF",    
        goanna_common:get_trace_abbreviation(return_from)
    ),    
    ?assertEqual(
        "EF",    
        goanna_common:get_trace_abbreviation(exception_from)
    ),    
    ?assertEqual(
        "SPW",    
        goanna_common:get_trace_abbreviation(spawn)
    ),    
    ?assertEqual(
        "EXI",    
        goanna_common:get_trace_abbreviation(exit)
    ),    
    ?assertEqual(
        "LI",    
        goanna_common:get_trace_abbreviation(link)
    ),    
    ?assertEqual(
        "ULI",    
        goanna_common:get_trace_abbreviation(unlink)
    ),    
    ?assertEqual(
        "GLI",    
        goanna_common:get_trace_abbreviation(getting_linked)
    ),    
    ?assertEqual(
        "GULI",    
        goanna_common:get_trace_abbreviation(getting_unlinked)
    ),    
    ?assertEqual(
        "REG",    
        goanna_common:get_trace_abbreviation(register)
    ),    
    ?assertEqual(
        "UNREG",    
        goanna_common:get_trace_abbreviation(unregister)
    ),    
    ?assertEqual(
        "I",    
        goanna_common:get_trace_abbreviation(in)
    ),    
    ?assertEqual(
        "O",    
        goanna_common:get_trace_abbreviation(out)
    ),    
    ?assertEqual(
        "GCS",    
        goanna_common:get_trace_abbreviation(gc_start)
    ),    
    ?assertEqual(
        "GCE",    
        goanna_common:get_trace_abbreviation(gc_end)
    ).

trace_abbreviations() ->
    ok = goanna_common:trace_abbreviations().

format_trace_item() ->
    ReportedTS = {_MegaSecs=1540, _Secs=666854, _MicroSecs=911643},
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host EF   : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), exception_from, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host RF   : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), return_from, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host C    : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), call, info, ReportedTS})
        )
    ),

    % ?assertEqual(
    %     "",
    %     lists:flatten(
    %         goanna_common:format_trace_item('n1@host', {trace_ts, self(), label, info, ReportedTS})
    %     )
    % ),

    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host REC  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), 'receive', info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host S    : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), send, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host STNEP: info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), send_to_non_existing_process, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host C    : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), call, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host RT   : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), return_to, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host RF   : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), return_from, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host EF   : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), exception_from, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host SPW  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), spawn, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host EXI  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), exit, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host LI   : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), link, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host ULI  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), unlink, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GLI  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), getting_linked, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GULI : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), getting_unlinked, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host REG  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), register, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host UNREG: info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), unregister, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host I    : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), in, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host O    : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), out, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GCS  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), gc_start, info, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GCE  : info\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), gc_end, info, ReportedTS})
        )
    ),




    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host EF   : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), exception_from, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host RF   : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), return_from, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host C    : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), call, info, extra, ReportedTS})
        )
    ),




    % ?assertEqual(
    %     "",
    %     lists:flatten(
    %         goanna_common:format_trace_item('n1@host', {trace_ts, self(), label, info, extra, ReportedTS})
    %     )
    % ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host REC  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), 'receive', info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host S    : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), send, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host STNEP: info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), send_to_non_existing_process, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host C    : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), call, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host RT   : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), return_to, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host RF   : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), return_from, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host EF   : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), exception_from, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host SPW  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), spawn, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host EXI  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), exit, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host LI   : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), link, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host ULI  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), unlink, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GLI  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), getting_linked, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GULI : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), getting_unlinked, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host REG  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), register, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host UNREG: info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), unregister, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host I    : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), in, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host O    : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), out, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GCS  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), gc_start, info, extra, ReportedTS})
        )
    ),
    ?assertEqual(
        "2018-10-27T19:00:54.911643 n1@host GCE  : info extra\n",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {trace_ts, self(), gc_end, info, extra, ReportedTS})
        )
    ),




    ?assertEqual(
        "n1@host {seq_trace,label,seq_trace_info}",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {seq_trace, label, seq_trace_info})
        )
    ),
    ?assertEqual(
        "n1@host {drop,1234}",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', {drop, 1234})
        )
    ),
    ?assertEqual(
        "n1@host bla",
        lists:flatten(
            goanna_common:format_trace_item('n1@host', bla)
        )
    ).

get_time() ->
    ?assertEqual(
        "2018-10-27T19:00:54.911643",
        goanna_common:get_time({_MegaSecs=1540, _Secs=666854, _MicroSecs=911643})
    ).