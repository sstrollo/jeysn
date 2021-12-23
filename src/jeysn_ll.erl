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
-module(jeysn_ll).

-export([init/0, init/1, init_string/1, init_string/2]).
-export([data/2, eof/1]).
-export([get_position/1, get_token_position/1, next_token/1, next_token/2]).

-export([encode_string/1, escape_string/1]).

%% tests
-export([debug/1]).
-export([file/1, tokenize/1]).
-export([json2_decode/1, json2_decode_file/1, json2_decode_stream/1]).

-on_load(nif_load/0).

%% ------------------------------------------------------------------------

-opaque jeysn_tokenizer() :: <<>>.
-export_type([jeysn_tokenizer/0]).

-type init_options() :: [init_option()] | init_option().
-type init_option() :: {'string', string_format()}.

-spec init(init_options()) -> jeysn_tokenizer().
init(Options) ->
    init_nif(init_options(Options)).

-spec init() -> jeysn_tokenizer().
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

-spec init_string(iodata()) -> jeysn_tokenizer().
init_string(String) ->
    init_string([], String).

-spec init_string(init_options(), iodata()) -> jeysn_tokenizer().
init_string(Options, String) ->
    S = init(Options),
    data(S, String),
    eof(S),
    S.

-spec data(jeysn_tokenizer(), iodata()|'eof') -> 'ok'.
data(_, _Data) ->
    nif_only().

-spec eof(jeysn_tokenizer()) -> 'ok'.
eof(State) ->
    data(State, 'eof').

-type json_token() :: '[' | '{' | '}' | ']' | ':' | ','
                    | 'false' | 'null' | 'true'
                    | number() | {'number', binary()}
                    | {'string', json_string_representation()}.
-type json_string_representation() :: json_string_type() |
                                      [json_string_type()|json_string_type()].
-type json_string_type() :: binary() | list() | atom().

-type string_format() :: string_format_type() | {string_format_type(), 0..127}.
-type string_format_type() :: 'binary' | 'string' | 'atom' | 'existing_atom'.
-spec next_token(jeysn_tokenizer(), string_format()) ->
                        json_token() |
                        {'error', json_token()|'error', position()} |
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

-type position() :: {Offset :: non_neg_integer(),
                     Line   :: non_neg_integer(),
                     Column :: non_neg_integer()}.

-spec get_position(jeysn_tokenizer()) ->
                          {position(), Before::binary(), After::binary()}.
%% @doc Return the current position of the tokenizer.
get_position(_State) ->
    nif_only().

-spec get_token_position(jeysn_tokenizer()) -> position().
%% @doc Return the position of the latest returned token.
get_token_position(_State) ->
    nif_only().

debug(_x) ->
    ok.

%% format_json_error(#json_error{} = Err) ->
%%     ErrStr = json_error_code_to_string(Err),
%%     io_lib:format("~s:~w:~w: ~s: ~s",
%%                   [Err#json_error.

%% ------------------------------------------------------------------------

-spec encode_string(iodata()) -> iodata().
encode_string(Str) ->
    [$", escape_string(Str), $"].

%% Note only characters required to be escaped are escaped. Any UTF-8
%% code points in the input string are assumed to be correct, and will
%% be returned as is.
-spec escape_string(iodata()) -> binary().
escape_string(_Str) ->
    nif_only().

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
        {error, Error} ->
            {error, Error, get_position(State)};
        Token ->
            io:format("Token: ~p\n", [Token]),
            floop(State, ReadF, [Token|Res])
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

nif_load() ->
    Dir =
        %% ../priv only works in test or src
        case application:get_application(?MODULE) of
            undefined ->
                "../priv";
            {ok, Application} ->
                case code:priv_dir(Application) of
                    {error,_} ->
                        "../priv";
                    PrivDir ->
                        PrivDir
                end
        end,
    erlang:load_nif(filename:append(Dir, "jeysn_ll"), 0).

nif_only() ->
    erlang:nif_error(not_loaded).
