-module(ejson).

-export([file/1, tokenize/1, xxx/1]).
-on_load(on_load/0).


file(FName) ->
    BufSz = 16,
    case file:open(FName, [read,binary,raw]) of
        {ok, Fd} ->
            F = fun () -> file:read(Fd, BufSz) end,
            ploop(init(42), F, [], F());
        _Err ->
            io:format("~p\n", [_Err]),
            _Err
    end.

tokenize(Buffer) ->
    S = init(42),
    tokenize(S, [], next_token(S, Buffer, 5, 0)).

tokenize(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokenize(S, Tokens, more) ->
    {incomplete, lists:reverse(Tokens), debug(S)};
tokenize(S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), debug(S), Err};
tokenize(S, Tokens, Token) ->
    tokenize(S, [Token|Tokens], next_token(S, [], 5, 0)).


xxx(Str) ->
    S = init(42),
    xxx_value(S, next_token(S, Str, 2, 0)).

xxx_value(S) ->
    xxx_value(S, xxx_next(S)).

%% looking-for: false/null/true/number/string/begin-array/begin-object
xxx_value(S, {token, Token}) ->
    case Token of
        false -> false;
        null -> null;
        true -> true;
        {string, Str} -> Str;
        '{' -> xxx_object(S);
        '[' -> xxx_array(S);
        N when is_number(N) -> N
    end.

%% looking-for: member/end-object
xxx_object(S) ->
    case xxx_next(S) of
        {token, '}'} ->
            {struct, []};
        Token = {token, _} ->
            xxx_object_member(S, Token, [])
    end.

xxx_object_member(S, {token, {string, Key}}, Members) ->
    case xxx_next(S) of
        {token, ':'} ->
            Value = xxx_value(S),
            xxx_object_next(S, [{Key, Value}|Members])
    end.

xxx_object_next(S, Members) ->
    case xxx_next(S) of
        {token, '}'} ->
            {struct, lists:reverse(Members)};
        {token, ','} ->
            xxx_object_member(S, xxx_next(S), Members)
    end.

xxx_array(S) ->
    case xxx_next(S) of
        {token, ']'} ->
            {array, []};
        Token = {token, _} ->
            Value = xxx_value(S, Token),
            xxx_array_next(S, [Value])
    end.

xxx_array_next(S, Array) ->
    case xxx_next(S) of
        {token, ']'} ->
            {array, lists:reverse(Array)};
        {token, ','} ->
            Value = xxx_value(S),
            xxx_array_next(S, [Value|Array])
    end.

xxx_next(S) ->
    next_token(S, [], 2, 0).


ploop(_State, _MoreBytesF, Res, eof) ->
    Res;
ploop(State, MoreBytesF, Res, {ok, Buf}) ->
    case next_token(State, Buf, 2, 0) of
        more ->
            _Debug = debug(State),
%            io:format("Debug: ~p\n", [debug(State)]),
            ploop(State, MoreBytesF, Res, MoreBytesF());
        eof ->
            _Debug = debug(State),
%            io:format("Debug: ~p\n", [debug(State)]),
            ploop(State, MoreBytesF, Res, MoreBytesF());
        {token, Symbol} ->
            io:format("Symbol: ~p\n", [Symbol]),
            ploop(State, MoreBytesF, [Symbol|Res], {ok, []});
        {error, Error} ->
            {error, Error, debug(State)}
    end.

next_token(_State, _Buf) ->
    next_token(_State, _Buf, 0, 0).

%% -> more_bytes | symbol | error
next_token(_State, _Buf, _StringAs, _MoreEnd) ->
    ok.

init(_X) ->
    ok.

debug(_x) ->
    ok.


on_load() ->
    erlang:load_nif("ejson_nif", 0).

