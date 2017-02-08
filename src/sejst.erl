-module(sejst).

-export([decode/1, decode_file/1, decode_stream/1]).
-export([json2_decode/1, json2_decode_file/1, json2_decode_stream/1]).

-import(ejson,
        [init/0, init_string/1, next_token/2, data/2, get_token_position/1]).

%% ------------------------------------------------------------------------

decode(Str) ->
    S = init_string(Str),
    decode_value(S, next_token(S, string)).

decode_file(FileName) ->
    BufSz = 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    F = fun () -> file:read(Fd, BufSz) end,
    decode_named_stream(F, FileName).

decode_stream(ReadFun) ->
    decode_value({init(), "-", ReadFun}).

decode_named_stream(ReadFun, Name) ->
    decode_value({init(), Name, ReadFun}).

decode_value(S) ->
    decode_value(S, decode_next(S)).

%% looking-for: false/null/true/number/string/begin-array/begin-object
decode_value(S, Token) ->
    case Token of
        false -> false;
        null -> null;
        true -> true;
        {string, Str} -> Str;
        '{' -> decode_object(S);
        '[' -> decode_array(S);
        N when is_number(N) -> N;
        Other -> decode_error(S, Other, [value])
    end.

%% looking-for: member/end-object
decode_object(S) ->
    case decode_next(S) of
        '}' ->
            #{};
        Token ->
            decode_object_member(S, Token, #{})
    end.

decode_object_member(S, {string, Key}, Object) ->
    case decode_next(S) of
        ':' ->
            Value = decode_value(S),
            decode_object_next(S, Object#{Key => Value});
        Other ->
            decode_error(S, Other, [':'])
    end;
decode_object_member(S, Other, _Object) ->
    decode_error(S, Other, [string]).

decode_object_next(S, Object) ->
    case decode_next(S) of
        '}' ->
            Object;
        ',' ->
            decode_object_member(S, decode_next(S), Object);
        Other ->
            decode_error(S, Other, ['}', ','])
    end.

decode_array(S) ->
    case decode_next(S) of
        ']' ->
            [];
        Token ->
            Value = decode_value(S, Token),
            decode_array_next(S, [Value])
    end.

decode_array_next(S, Array) ->
    case decode_next(S) of
        ']' ->
            lists:reverse(Array);
        ',' ->
            Value = decode_value(S),
            decode_array_next(S, [Value|Array]);
        Other ->
            decode_error(S, Other, [']', ','])
    end.

decode_next({S, _, ReadF} = State) ->
    case next_token(S, string) of
        more ->
            file_data(S, ReadF()),
            decode_next(State);
        Token ->
            Token
    end;
decode_next(S) ->
    next_token(S, string).

decode_error(S, Got, Expected) ->
    {File, Line, Col} = get_position(S),
    io:format("~s:~w:~w: Error: got ~s expected: ~w\n",
              [File, Line, Col, Got, Expected]),
    erlang:error(syntax).

get_position({S, File, _}) ->
    {_, Line, Col} = get_token_position(S),
    {File, Line, Col};
get_position(S) ->
    {_, Line, Col} = get_token_position(S),
    {"-", Line, Col}.

%% ------------------------------------------------------------------------

json2_decode(Str) ->
    S = init_string(Str),
    json2_decode_value(S, next_token(S, string)).

json2_decode_file(FileName) ->
    BufSz = 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    F = fun () -> file:read(Fd, BufSz) end,
    json2_decode_named_stream(F, FileName).

json2_decode_stream(ReadFun) ->
    json2_decode_value({init(), "-", ReadFun}).

json2_decode_named_stream(ReadFun, Name) ->
    json2_decode_value({init(), Name, ReadFun}).

json2_decode_value(S) ->
    json2_decode_value(S, json2_decode_next(S)).

%% looking-for: false/null/true/number/string/begin-array/begin-object
json2_decode_value(S, Token) ->
    case Token of
        false -> false;
        null -> null;
        true -> true;
        {string, Str} -> Str;
        '{' -> json2_decode_object(S);
        '[' -> json2_decode_array(S);
        N when is_number(N) -> N;
        Other -> json2_decode_error(S, Other, [value])
    end.

%% looking-for: member/end-object
json2_decode_object(S) ->
    case json2_decode_next(S) of
        '}' ->
            {struct, []};
        Token ->
            json2_decode_object_member(S, Token, [])
    end.

json2_decode_object_member(S, {string, Key}, Members) ->
    case json2_decode_next(S) of
        ':' ->
            Value = json2_decode_value(S),
            json2_decode_object_next(S, [{Key, Value}|Members]);
        Other ->
            json2_decode_error(S, Other, [':'])
    end;
json2_decode_object_member(S, Other, _Members) ->
    json2_decode_error(S, Other, [string]).

json2_decode_object_next(S, Members) ->
    case json2_decode_next(S) of
        '}' ->
            {struct, lists:reverse(Members)};
        ',' ->
            json2_decode_object_member(S, json2_decode_next(S), Members);
        Other ->
            json2_decode_error(S, Other, ['}', ','])
    end.

json2_decode_array(S) ->
    case json2_decode_next(S) of
        ']' ->
            {array, []};
        Token ->
            Value = json2_decode_value(S, Token),
            json2_decode_array_next(S, [Value])
    end.

json2_decode_array_next(S, Array) ->
    case json2_decode_next(S) of
        ']' ->
            {array, lists:reverse(Array)};
        ',' ->
            Value = json2_decode_value(S),
            json2_decode_array_next(S, [Value|Array]);
        Other ->
            json2_decode_error(S, Other, [']', ','])
    end.

json2_decode_next({S, _, ReadF} = State) ->
    case next_token(S, string) of
        more ->
            file_data(S, ReadF()),
            json2_decode_next(State);
        Token ->
            Token
    end;
json2_decode_next(S) ->
    next_token(S, string).

json2_decode_error(S, Got, Expected) ->
    {File, Line, Col} = json2_get_position(S),
    io:format("~s:~w:~w: Error: got ~s expected: ~w\n",
              [File, Line, Col, Got, Expected]),
    erlang:error(syntax).

json2_get_position({S, File, _}) ->
    {_, Line, Col} = get_token_position(S),
    {File, Line, Col};
json2_get_position(S) ->
    {_, Line, Col} = get_token_position(S),
    {"-", Line, Col}.


%% ------------------------------------------------------------------------

file_data(S, {ok, Buf}) ->
    data(S, Buf);
file_data(S, eof) ->
    data(S, eof).
