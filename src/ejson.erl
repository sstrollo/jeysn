%%
%%  The JavaScript Object Notation (JSON) Data Interchange Format
%%
%%  https://tools.ietf.org/html/rfc7159
%%
%%
%%  JSON Encoding of Data Modeled with YANG
%%
%%  https://tools.ietf.org/html/draft-ietf-netmod-yang-json-02
%%
-module(ejson).

-export([init/0, init/1, init_string/1, init_string/2]).
-export([data/2, eof/1, get_position/1, next_token/1, next_token/2]).

%% tests
-export([debug/1]).
-export([file/1, tokenize/1, xxx/1, xxxf/1, xxxrf/1]).

-on_load(nif_load/0).

%% ------------------------------------------------------------------------

-opaque ejson_tokenizer() :: <<>>.
-export_type([ejson_tokenizer/0]).

-type init_options() :: [init_option()] | init_option().
-type init_option() :: {'string', string_format()}.

-spec init(init_options()) -> ejson_tokenizer().
init(Options) ->
    init_nif(init_options(Options)).

-spec init() -> ejson_tokenizer().
init() ->
    nif_only().

init_options([{string, StringFormat}|T]) ->
    [{string, string_format_internal(StringFormat)}|init_options(T)];
init_options([H|T]) ->
    [H|init_options(T)];
init_options([]) ->
    [];
init_options(Option) ->
    init_options([Option]).

init_nif(_Options) ->
    nif_only().

-spec init_string(iodata()) -> ejson_tokenizer().
init_string(String) ->
    init_string([], String).

-spec init_string(init_options(), iodata()) -> ejson_tokenizer().
init_string(Options, String) ->
    S = init(Options),
    data(S, String),
    eof(S),
    S.

-spec data(ejson_tokenizer(), iodata()|'eof') -> 'ok'.
data(_, _Data) ->
    nif_only().

-spec eof(ejson_tokenizer()) -> 'ok'.
eof(State) ->
    data(State, 'eof').

-type string_format() :: string_format_type() | {string_format_type(), 0..127}.
-type string_format_type() :: 'binary' | 'string' | 'atom' | 'existing_atom'.
-spec next_token(ejson_tokenizer(), string_format()) ->
                        {'token', term()} |
                        {'error', term(), position()} |
                        more |
                        eof.
next_token(State, StringFormat) ->
    next_token_nif(State, string_format_internal(StringFormat)).

next_token(State) ->
    next_token_nif(State).

next_token_nif(_State) ->
    nif_only().

next_token_nif(_State, _InternalStringFormat) ->
    nif_only().

string_format_internal({Format, SplitChar}) when SplitChar >= 0,
                                                 SplitChar =< 127 ->
    {string_format_internal(Format), SplitChar};
string_format_internal('binary')        -> 0;
string_format_internal('string')        -> 1;
string_format_internal('atom')          -> 2;
string_format_internal('existing_atom') -> 3.

-type position() :: {{Pos    :: non_neg_integer(),
                      Line   :: non_neg_integer(),
                      Column :: non_neg_integer()},
                     Before::binary(), After::binary()}.
-spec get_position(ejson_tokenizer()) -> position().
get_position(_State) ->
    nif_only().

debug(_x) ->
    ok.

%% ------------------------------------------------------------------------

file(FName) ->
    BufSz = 16,
    case file:open(FName, [read,binary,raw]) of
        {ok, Fd} ->
            F = fun () -> file:read(Fd, BufSz) end,
            floop(init(), F, []);
        _Err ->
            io:format("~p\n", [_Err]),
            _Err
    end.

file_data(S, {ok, Buf}) ->
    data(S, Buf);
file_data(S, eof) ->
    data(S, eof).

floop(State, ReadF, Res) ->
    case next_token(State, string) of
        more ->
            _Debug = debug(State),
%            io:format("Debug: ~p\n", [debug(State)]),
            file_data(State, ReadF()),
            floop(State, ReadF, Res);
        eof ->
            _Debug = debug(State),
%            io:format("Debug: ~p\n", [debug(State)]),
            lists:reverse(Res);
        {token, Token} ->
            io:format("Token: ~p\n", [Token]),
            floop(State, ReadF, [Token|Res]);
        {error, Error} ->
            {error, Error, get_position(State)}
    end.


tokenize(Buffer) ->
    S = init_string(Buffer),
    tokenize(S, [], next_token(S, {atom,$:})).

tokenize(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokenize(S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), debug(S), Err};
tokenize(S, Tokens, Token) ->
    tokenize(S, [Token|Tokens], next_token(S, {atom,$:})).


xxx(Str) ->
    S = init_string(Str),
    xxx_value(S, next_token(S, string)).

xxxf(FileName) ->
    BufSz = 16,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    F = fun () -> file:read(Fd, BufSz) end,
    xxxrf(F).

xxxrf(ReadFun) ->
    xxx_value({init(), ReadFun}).

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
    case next_token(S, string) of
        more ->
            file_data(S, ReadF()),
            xxx_next({S, ReadF});
        Token ->
            Token
    end;
xxx_next(S) ->
    next_token(S, string).


%% ------------------------------------------------------------------------

nif_load() ->
    case code:priv_dir(tts) of
        {error,_} ->
            erlang:load_nif(filename:append("../priv", "ejson_nif"), 0);
        PrivDir ->
            erlang:load_nif(filename:append(PrivDir, "ejson_nif"), 0)
    end.

nif_only() ->
    erlang:nif_error(not_loaded).
