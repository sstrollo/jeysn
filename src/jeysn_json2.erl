%%
%% Copyright Sebastian Strollo <seb@strollo.org>
%% SPDX-License-Identifier: Apache-2.0
%%
-module(jeysn_json2).
%%
%% Decode into structures compatible with
%% https://github.com/erlyaws/yaws/blob/master/src/json2.erl
%% (should you ever need it:)
%%
-export([decode/1, decode_file/1, decode_stream/1]).

-import(jeysn_ll,
        [init/0, init/1, init_string/1, init_string/2,
         next_token/1, data/2, get_token_position/1]).

%% ------------------------------------------------------------------------

decode(Str) ->
    S = init_string([{string,string}], Str),
    decode_value(S, next_token(S)).

decode_file(FileName) ->
    BufSz = 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    F = fun () -> file:read(Fd, BufSz) end,
    decode_named_stream(F, FileName).

decode_stream(ReadFun) ->
    decode_value({init([{string,string}]), "-", ReadFun}).

decode_named_stream(ReadFun, Name) ->
    decode_value({init([{string,string}]), Name, ReadFun}).

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
        {number, IntegerBinary} -> binary_to_integer(IntegerBinary);
        Other -> decode_error(S, Other, [value])
    end.

%% looking-for: member/end-object
decode_object(S) ->
    case decode_next(S) of
        '}' ->
            {struct, []};
        Token ->
            decode_object_member(S, Token, [])
    end.

decode_object_member(S, {string, Key}, Members) ->
    case decode_next(S) of
        ':' ->
            Value = decode_value(S),
            decode_object_next(S, [{Key, Value}|Members]);
        Other ->
            decode_error(S, Other, [':'])
    end;
decode_object_member(S, Other, _Members) ->
    decode_error(S, Other, [string]).

decode_object_next(S, Members) ->
    case decode_next(S) of
        '}' ->
            {struct, lists:reverse(Members)};
        ',' ->
            decode_object_member(S, decode_next(S), Members);
        Other ->
            decode_error(S, Other, ['}', ','])
    end.

decode_array(S) ->
    case decode_next(S) of
        ']' ->
            {array, []};
        Token ->
            Value = decode_value(S, Token),
            decode_array_next(S, [Value])
    end.

decode_array_next(S, Array) ->
    case decode_next(S) of
        ']' ->
            {array, lists:reverse(Array)};
        ',' ->
            Value = decode_value(S),
            decode_array_next(S, [Value|Array]);
        Other ->
            decode_error(S, Other, [']', ','])
    end.

decode_next({S, _, ReadF} = State) ->
    case next_token(S) of
        more ->
            file_data(S, ReadF()),
            decode_next(State);
        Token ->
            Token
    end;
decode_next(S) ->
    next_token(S).

decode_error(S, Got, Expected) ->
    {File, Line, Col} = get_position(S),
    io:format("~s:~w:~w: Error: got ~p expected: ~w\n",
              [File, Line, Col, Got, Expected]),
    erlang:error(syntax).

get_position({S, File, _}) ->
    {_, Line, Col} = get_token_position(S),
    {File, Line, Col};
get_position(S) ->
    {_, Line, Col} = get_token_position(S),
    {"-", Line, Col}.

%% ------------------------------------------------------------------------

file_data(S, {ok, Buf}) ->
    data(S, Buf);
file_data(S, eof) ->
    data(S, eof).
