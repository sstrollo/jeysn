%%
%% Copyright Sebastian Strollo <seb@strollo.org>
%% SPDX-License-Identifier: Apache-2.0
%%
-module(jeysn).

-export([decode/1, decode_file/1, decode_stream/1]).
-export([encode/1, encode/2]).

-import(jeysn_ll,
        [init/0, init/1, init_string/1, init_string/2,
         data/2, get_token_position/1]).

%% ------------------------------------------------------------------------

-record(ds, {
          tokenizer
          , filename = <<"-">>
          , string_format = binary
          , name_format = existing_atom
          , more = fun () -> eof end
         }).


decode(Str) ->
    S = #ds{tokenizer = init_string(Str)},
    decode_value(S).

decode_file(FileName) ->
    BufSz = 64*1024, % 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    S = #ds{tokenizer = init(),
            filename = FileName,
            more = fun () -> file:read(Fd, BufSz) end},
    decode_value(S).

decode_stream(ReadFun) ->
    S = #ds{tokenizer = init(),
            more = ReadFun},
    decode_value(S).

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

%% looking-for: pair-name/end-object
decode_object(S) ->
    case decode_next(S, S#ds.name_format) of
        '}' ->
            #{};
        Token ->
            decode_object_pair(S, Token, #{})
    end.

decode_object_pair(S, {string, Name}, Object) ->
    case decode_next(S) of
        ':' ->
            Value = decode_value(S),
            decode_object_next(S, Object#{Name => Value});
        Other ->
            decode_error(S, Other, [':'])
    end;
decode_object_pair(S, Other, _Object) ->
    decode_error(S, Other, [string]).

decode_object_next(S, Object) ->
    case decode_next(S) of
        '}' ->
            Object;
        ',' ->
            decode_object_pair(S, decode_next(S, S#ds.name_format), Object);
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

decode_next(S) ->
    decode_next(S, S#ds.string_format).

decode_next(S, StrFmt) ->
    case jeysn_ll:next_token(S#ds.tokenizer, StrFmt) of
        more ->
            case (S#ds.more)() of
                {ok, Data} ->
                    jeysn_ll:data(S#ds.tokenizer, Data),
                    decode_next(S, StrFmt);
                eof ->
                    jeysn_ll:eof(S#ds.tokenizer),
                    decode_next(S, StrFmt);
                {error, _} = _Err ->
                    {error, more_error,
                     jeysn_ll:get_position(S#ds.tokenizer)}
            end;
        {number, IntegerBinary} ->
            binary_to_integer(IntegerBinary);
        TokenOrErrorOrEOF ->
            TokenOrErrorOrEOF
    end.

decode_error(S, Got, Expected) ->
    {File, Line, Col} = get_position(S),
    io:format("~s:~w:~w: Error: got ~p expected: ~w\n",
              [File, Line, Col, Got, Expected]),
    erlang:error(syntax).

get_position(#ds{tokenizer = T, filename = File}) ->
    {_, Line, Col} = get_token_position(T),
    {File, Line, Col}.

%% ------------------------------------------------------------------------

encode(Term) ->
    encode(Term, []).

encode(false, _) -> <<"false">>;
encode(null, _) -> <<"null">>;
encode(true, _) -> <<"true">>;
encode(N, _) when is_integer(N) -> integer_to_list(N);
encode(N, _) when is_float(N) -> float_to_list(N);

encode(Object, _) when map_size(Object) =:= 0 ->
    <<"{}">>;
encode(Object, X) when is_map(Object) ->
    [${, encode_sequence(maps:to_list(Object), X), $}];
encode({Key, Value}, X) ->
    [encode(Key, X), $:, encode(Value, X)];
encode(Str, _) when is_atom(Str) ->
    jeysn_ll:encode_string(atom_to_binary(Str, utf8));
encode(Str, _) when is_binary(Str) ->
    jeysn_ll:encode_string(Str);

encode("", _) ->
    <<"\"\"">>;
encode(Item, X) when is_list(Item) ->
    case is_string(Item) of
        false ->
            [$[, encode_sequence(Item, X), $]];
        true ->
            jeysn_ll:encode_string(Item)
    end.

encode_sequence([Item], X) ->
    encode(Item, X);
encode_sequence([Item|Rest], X) ->
    [encode(Item, X), $,|encode_sequence(Rest, X)].

is_string([]) ->
    true;
is_string([Char|Chars]) when is_integer(Char)
                             andalso (Char < 127)
                             andalso (Char > 31) ->
    is_string(Chars);
is_string(_) ->
    false.
