-module(read_coverdata).
-compile(export_all).

d() ->
    dec_cov_data("_build/test/cover/eunit.coverdata").

dec_cov_data(Filename) ->
    {ok,FPID} = file:open(Filename, [read, raw, binary]),
    loop(FPID, read_first_byte_length(FPID)).

loop(_FPID, eof) ->
    eof;
loop(_FPID, {error, R}) ->
    {error, R};
loop(FPID, {ok, {'$size', X}}) ->
    loop(FPID, file:read(FPID, X));
loop(FPID, {ok, Data}) ->
    io:format("~p~n", [binary_to_term(Data)]),
    case binary_to_term(Data) of
        {'$size',S} ->
            loop(FPID, file:read(FPID, S));
        _ ->
            loop(FPID, read_first_byte_length(FPID))
    end.

read_first_byte_length(FPID) ->
     case first_byte_size_int(FPID) of
         FirstByteSize when is_integer(FirstByteSize) ->
             file:read(FPID, FirstByteSize);
         R ->
             R
     end.

first_byte_size_int(FPID) ->
    case file:read(FPID, 1) of
        {ok, FirstByteBinString} ->
            [Int] = io_lib:format("~w", binary_to_list(FirstByteBinString)),
            list_to_integer(Int);
        R ->
            R
    end.