-module(ejson).

-export([new/1, data/2, next_token/2]).

%% tests
-export([debug/1]).
-export([file/1, tokenize/1, xxx/1, xxxf/1]).

-on_load(nif_load/0).

%% ------------------------------------------------------------------------

-opaque ejson_tokenizer() :: <<>>.
-export_type([ejson_tokenizer/0]).

-spec new(term()) -> ejson_tokenizer().
new(_X) ->
    nif_only().

new(_X, String) ->
    S = new(_X),
    data(S, String),
    data(S, eof),
    S.

-spec data(ejson_tokenizer(), iodata()|'eof') -> 'ok'.
data(_, _Data) ->
    nif_only().

-type string_format() :: 0..255.
-spec next_token(ejson_tokenizer(), string_format()) ->
                        {'token', term()} | {'error', term()} | more | eof.
next_token(_State, _StringFormat) ->
    nif_only().



debug(_x) ->
    ok.

%% ------------------------------------------------------------------------

file(FName) ->
    BufSz = 16,
    case file:open(FName, [read,binary,raw]) of
        {ok, Fd} ->
            F = fun () -> file:read(Fd, BufSz) end,
            floop(new(42), F, []);
        _Err ->
            io:format("~p\n", [_Err]),
            _Err
    end.

file_data(S, {ok, Buf}) ->
    data(S, Buf);
file_data(S, eof) ->
    data(S, eof).

floop(State, ReadF, Res) ->
    case next_token(State, 2) of
        more ->
            _Debug = debug(State),
%            io:format("Debug: ~p\n", [debug(State)]),
            file_data(State, ReadF()),
            floop(State, ReadF, Res);
        eof ->
            _Debug = debug(State),
%            io:format("Debug: ~p\n", [debug(State)]),
            lists:reverse(Res);
        {token, Symbol} ->
            io:format("Symbol: ~p\n", [Symbol]),
            floop(State, ReadF, [Symbol|Res]);
        {error, Error} ->
            {error, Error, debug(State)}
    end.


tokenize(Buffer) ->
    S = new(42, Buffer),
    tokenize(S, [], next_token(S, 5)).

tokenize(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokenize(S, Tokens, more) ->
    {incomplete, lists:reverse(Tokens), debug(S)};
tokenize(S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), debug(S), Err};
tokenize(S, Tokens, Token) ->
    tokenize(S, [Token|Tokens], next_token(S, 5)).


xxx(Str) ->
    S = new(42, Str),
    xxx_value(S, next_token(S, 2)).

xxxf(FileName) ->
    BufSz = 16,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    F = fun () -> file:read(Fd, BufSz) end,
    S = new(42),
    xxx_value({S, F}).


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

xxx_next({S, ReadF}) ->
    case next_token(S, 2) of
        more ->
            file_data(S, ReadF()),
            xxx_next({S, ReadF});
        Token ->
            Token
    end;
xxx_next(S) ->
    next_token(S, 2).


%% ------------------------------------------------------------------------

nif_load() ->
    erlang:load_nif("ejson_nif", 0).

nif_only() ->
    erlang:nif_error(not_loaded).
