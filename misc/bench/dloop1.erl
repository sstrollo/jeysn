-module(dloop1).

-export([decode/1, decode_file/1, decode_stream/1]).

-record(bench, {
          tokenizer
          , filename = <<"-">>
          , string_format = binary
          , name_format = existing_atom
          , more = fun () -> eof end
         }).

decode(Str) ->
    S = #bench{tokenizer = jeysn_ll:init_string(Str)},
    value(S).

decode_file(FileName) ->
    BufSz = 64*1024, % 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    S = #bench{tokenizer = jeysn_ll:init(),
            filename = FileName,
            more = fun () -> file:read(Fd, BufSz) end},
    value(S).

decode_stream(ReadFun) ->
    S = #bench{tokenizer = jeysn_ll:init(),
            more = ReadFun},
    value(S).

value(S) ->
    value(next_token(S), S).

%% looking-for: false/null/true/number/string/begin-array/begin-object
value(Token, S) ->
    case Token of
        false -> ok;
        null -> ok;
        true -> ok;
        {string, _Str} -> ok;
        N when is_number(N) -> ok;
        '{' -> object(S);
        '[' -> array(S);
        Other -> raise_error(S, Other, [value])
    end.

%% looking-for: pair-name/end-object
object(S) ->
    case next_token(S, S#bench.name_format) of
        '}' ->
            ok;
        Token ->
            object_pair(S, Token)
    end.

object_pair(S, {string, _Name}) ->
    case next_token(S) of
        ':' ->
            value(S),
            object_next(S);
        Other ->
            raise_error(S, Other, [':'])
    end;
object_pair(S, Other) ->
    raise_error(S, Other, [string]).

object_next(S) ->
    case next_token(S) of
        '}' ->
            ok;
        ',' ->
            object_pair(S, next_token(S, S#bench.name_format));
        Other ->
            raise_error(S, Other, ['}', ','])
    end.

array(S) ->
    case next_token(S) of
        ']' ->
            ok;
        Token ->
            value(Token, S),
            array_next(S)
    end.

array_next(S) ->
    case next_token(S) of
        ']' ->
            ok;
        ',' ->
            value(S),
            array_next(S);
        Other ->
            raise_error(S, Other, [']', ','])
    end.

next_token(S) ->
    next_token(S, S#bench.string_format).

next_token(S, StrFmt) ->
    case jeysn_ll:next_token(S#bench.tokenizer, StrFmt) of
        more ->
            case (S#bench.more)() of
                {ok, Data} ->
                    jeysn_ll:data(S#bench.tokenizer, Data),
                    next_token(S, StrFmt);
                eof ->
                    jeysn_ll:eof(S#bench.tokenizer),
                    next_token(S, StrFmt);
                {error, _} = _Err ->
                    {error, more_error,
                     jeysn_ll:get_position_info(S#bench.tokenizer)}
            end;
        {number, IntegerBinary} ->
            binary_to_integer(IntegerBinary);
        TokenOrErrorOrEOF ->
            TokenOrErrorOrEOF
    end.

raise_error(S, Got, Expected) ->
    {File, Line, Col} = get_position(S),
    io:format("~s:~w:~w: Error: got ~p expected: ~w\n",
              [File, Line, Col, Got, Expected]),
    erlang:error(syntax).

get_position(#bench{tokenizer = T, filename = File}) ->
    {_, Line, Col} = jeysn_ll:get_token_position(T),
    {File, Line, Col}.
