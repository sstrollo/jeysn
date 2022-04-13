%%
%% Experimental and debugging stuff
%%
-module(wip).

-compile(export_all).

%% ------------------------------------------------------------------------

file(FName) ->
    BufSz = 16,
    case file:open(FName, [read,binary,raw]) of
        {ok, Fd} ->
            F = fun () -> file:read(Fd, BufSz) end,
            floop(jeysn_ll:init(), F, []);
        _Err ->
            io:format("~p\n", [_Err]),
            _Err
    end.

file_data(S, {ok, Buf}) ->
    jeysn_ll:data(S, Buf);
file_data(S, eof) ->
    jeysn_ll:data(S, eof).

floop(State, ReadF, Res) ->
    case jeysn_ll:next_token(State, string) of
        more ->
            _Debug = jeysn_ll:debug(State),
%            io:format("Debug: ~p\n", [_Debug]),
            file_data(State, ReadF()),
            floop(State, ReadF, Res);
        eof ->
            _Debug = jeysn_ll:debug(State),
%            io:format("Debug: ~p\n", [_Debug]),
            lists:reverse(Res);
        {error, Error} ->
            {error, Error, jeysn_ll:get_position_info(State)};
        Token ->
            io:format("Token: ~p\n", [Token]),
            floop(State, ReadF, [Token|Res])
    end.

%% ------------------------------------------------------------------------

t() ->
    tokenize(<<"{\"foo:bar\": 42}">>).

tokenize(Buffer) ->
    S = jeysn_ll:init_string(Buffer),
    tokenize(S, [], jeysn_ll:next_token(S, {existing_atom,$:})).

tokenize(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokenize(S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), jeysn_ll:debug(S), Err};
tokenize(S, Tokens, Token) ->
    tokenize(S, [Token|Tokens], jeysn_ll:next_token(S, {existing_atom,$:})).

%% ------------------------------------------------------------------------

%% -record(ds, {
%%           tokenizer
%%           , filename = <<"-">>
%%           , string_format = binary
%%           , name_format = existing_atom
%%           , more = fun () -> eof end
%%          }).


%% decode(Str) ->
%%     S = #ds{tokenizer = jeysn_ll:init_string(Str)},
%%     decode_value(S).

%% decode_file(FileName) ->
%%     BufSz = 64*1024, % 8192,
%%     {ok, Fd} = file:open(FileName, [read,binary,raw]),
%%     S = #ds{tokenizer = jeysn_ll:init(),
%%             filename = FileName,
%%             more = fun () -> file:read(Fd, BufSz) end},
%%     decode_value(S).

%% decode_stream(ReadFun) ->
%%     S = #ds{tokenizer = jeysn_ll:init(),
%%             more = ReadFun},
%%     decode_value(S).

%% decode_value(S, EHS, EHC, Next) ->
%%     decode_value(decode_get_token(S), S, EHS, EHC, Next).

%% %% looking-for: false/null/true/number/string/begin-array/begin-object
%% decode_value('{', S, EHS, EHC) ->
%%     decode_object(S, event('{', S, EHS, EHC), EHC);
%% decode_value('[', S, EHS, EHC) ->
%%     decode_array(S, event('[', S, EHS, EHC), EHC);
%% decode_value(Token, S, EHS, EHC) ->
%%     case Token of
%%         false ->
%%             Next(S, event(Token, S, EHS, EHC), EHC);
%%         null ->
%%             Next(S, event(Token, S, EHS, EHC), EHC);
%%         true ->
%%             Next(S, event(Token, S, EHS, EHC), EHC);
%%         {string, Str} ->
%%             Next(S, event(Token, S, EHS, EHC), EHC);
%%         N when is_number(N) ->
%%             Next(S, event(Token, S, EHS, EHC), EHC);
%%         Other ->
%%             decode_error(S, Other, [value])
%%     end.

%% -define(n(__F, __N),
%%         fun (__S, __EHS, __EHC) -> (fun __F/4)(__S, __EHS, __EHS, __N) end).

%% %% looking-for: pair-name/end-object
%% decode_object(S, EHS, EHC) ->
%%     case decode_get_token(S, S#ds.name_format) of
%%         '}' ->
%%             event('}', S, EHS, EHC);
%%         Token ->
%%             decode_object_pair(Token, S, EHS, EHC)
%%     end.

%% decode_object_pair(S, {string, Name}, Object) ->
%%     case decode_get_token(S) of
%%         ':' ->
%%             decode_value(S, event({name, Name}, S, EHS, EHC), EHC,
%%                          ?n(decode_object_next, Next));
%%         Other ->
%%             decode_error(S, Other, [':'])
%%     end;
%% decode_object_pair(S, Other, _Object) ->
%%     decode_error(S, Other, [string]).


%% decode_object_next(S, EHS, EHC) ->
%%     case decode_get_token(S) of
%%         '}' ->
%%             Object;
%%         ',' ->
%%             decode_object_pair(S, decode_get_token(S, S#ds.name_format), Object);
%%         Other ->
%%             decode_error(S, Other, ['}', ','])
%%     end.

%% decode_array(S) ->
%%     case decode_get_token(S) of
%%         ']' ->
%%             [];
%%         Token ->
%%             Value = decode_value(S, Token),
%%             decode_array_next(S, [Value])
%%     end.

%% decode_array_next(S, Array) ->
%%     case decode_get_token(S) of
%%         ']' ->
%%             lists:reverse(Array);
%%         ',' ->
%%             Value = decode_value(S),
%%             decode_array_next(S, [Value|Array]);
%%         Other ->
%%             decode_error(S, Other, [']', ','])
%%     end.

%% decode_get_token(S) ->
%%     decode_get_token(S, S#ds.string_format).

%% decode_get_token(S, StrFmt) ->
%%     case jeysn_ll:next_token(S#ds.tokenizer, StrFmt) of
%%         more ->
%%             case (S#ds.more)() of
%%                 {ok, Data} ->
%%                     jeysn_ll:data(S#ds.tokenizer, Data),
%%                     decode_get_token(S, StrFmt);
%%                 eof ->
%%                     jeysn_ll:eof(S#ds.tokenizer),
%%                     decode_get_token(S, StrFmt);
%%                 {error, _} = _Err ->
%%                     {error, more_error,
%%                      jeysn_ll:get_position_info(S#ds.tokenizer)}
%%             end;
%%         {number, IntegerBinary} ->
%%             binary_to_integer(IntegerBinary);
%%         TokenOrErrorOrEOF ->
%%             TokenOrErrorOrEOF
%%     end.

%% decode_error(S, Got, Expected) ->
%%     {File, Line, Col} = decode_get_position(S),
%%     io:format("~s:~w:~w: Error: got ~p expected: ~w\n",
%%               [File, Line, Col, Got, Expected]),
%%     erlang:error(syntax).

%% decode_get_position(#ds{tokenizer = T, filename = File}) ->
%%     {_, Line, Col} = jeysn_ll:get_token_position(T),
%%     {File, Line, Col}.

%% %% ------------------------------------------------------------------------

