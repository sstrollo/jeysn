
-export([decode/1, decode_file/1, decode_stream/1]).

-record(dl, {
          tokenizer
          , filename = <<"-">>
          , string_format = binary
          , name_format = binary % existing_atom
          , more = fun () -> eof end
         }).

decode(Str) ->
    S = #dl{tokenizer = jeysn_ll:init_string(Str)},
    value(S).

decode_file(FileName) ->
    BufSz = 64*1024, % 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    S = #dl{tokenizer = jeysn_ll:init(),
            filename = FileName,
            more = fun () -> file:read(Fd, BufSz) end},
    value(S).

decode_stream(ReadFun) ->
    S = #dl{tokenizer = jeysn_ll:init(),
            more = ReadFun},
    value(S).

value(S) ->
    value(next_token(S), S, ?vinit).

value(S, Next) ->
    value(next_token(S), S, Next).

%% looking-for: false/null/true/number/string/begin-array/begin-object
value(Token, S, Next) ->
    case Token of
        false -> ?cn(Next, S);
        null -> ?cn(Next, S);
        true -> ?cn(Next, S);
        {string, _Str} -> ?cn(Next, S);
        N when is_number(N) -> ?cn(Next, S);
        '{' -> object(S, Next);
        '[' -> array(S, Next);
        Other -> raise_error(S, Other, [value])
    end.

%% looking-for: pair-name/end-object
object(S, Next) ->
    case next_token(S, S#dl.name_format) of
        '}' ->
            ok;
        Token ->
            object_pair(S, Token, Next)
    end.

object_pair(S, {string, _Name}, Next) ->
    case next_token(S) of
        ':' ->
            value(S, ?n(object_next, Next));
        Other ->
            raise_error(S, Other, [':'])
    end;
object_pair(S, Other, _Next) ->
    raise_error(S, Other, [string]).

object_next(S, Next) ->
    case next_token(S) of
        '}' ->
            ?cn(Next, S);
        ',' ->
            object_pair(S, next_token(S, S#dl.name_format), Next);
        Other ->
            raise_error(S, Other, ['}', ','])
    end.

array(S, Next) ->
    case next_token(S) of
        ']' ->
            ?cn(Next, S);
        Token ->
            value(Token, S, ?n(array_next, Next))
    end.

array_next(S, Next) ->
    case next_token(S) of
        ']' ->
            ?cn(Next, S);
        ',' ->
            value(S, ?n(array_next, Next));
        Other ->
            raise_error(S, Other, [']', ','])
    end.

next_token(S) ->
    next_token(S, S#dl.string_format).

next_token(S, StrFmt) ->
    case jeysn_ll:next_token(S#dl.tokenizer, StrFmt) of
        more ->
            case (S#dl.more)() of
                {ok, Data} ->
                    jeysn_ll:data(S#dl.tokenizer, Data),
                    next_token(S, StrFmt);
                eof ->
                    jeysn_ll:eof(S#dl.tokenizer),
                    next_token(S, StrFmt);
                {error, _} = _Err ->
                    {error, more_error,
                     jeysn_ll:get_position(S#dl.tokenizer)}
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

get_position(#dl{tokenizer = T, filename = File}) ->
    {_, Line, Col} = jeysn_ll:get_token_position(T),
    {File, Line, Col}.
