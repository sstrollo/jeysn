%%
%% Copyright Sebastian Strollo <seb@strollo.org>
%% SPDX-License-Identifier: Apache-2.0
%%
%% The low-level tokenizer (implemented as a NIF).
%%
-module(jeysn_ll).

-export([init/0, init/1, init_string/1, init_string/2]).
-export([data/2, eof/1]).
-export([get_position/1, get_token_position/1, get_remaining_data/1]).
-export([next_token/1, next_token/2]).

-export([encode_string/1, escape_string/1]).

%% tests
-export([debug/1]).

-on_load(nif_load/0).

%% ------------------------------------------------------------------------

-opaque jeysn_tokenizer() :: reference().
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
    init_string(String, []).

-spec init_string(iodata(), init_options()) -> jeysn_tokenizer().
init_string(String, Options) ->
    S = init(Options),
    data(S, String),
    eof(S),
    S.

-spec data(jeysn_tokenizer(), iodata()|'eof') -> 'ok' | {'error', 'eof'}.
%% @doc Add more iodata to the tokenizer. To indicate that there is no
%% more data to be found use the atom 'eof' as data. Returns {'error',
%% 'eof'} if the Tokenizer is already in eof state.
data(_, _Data) ->
    nif_only().

-spec eof(jeysn_tokenizer()) -> 'ok'.
%% @equiv data(Tokenizer, 'eof')
eof(Tokenizer) ->
    data(Tokenizer, 'eof').

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
                        json_token()
                      | {'error', json_token()|'error', position()}
                      | 'more'
                      | 'eof'
                      .
next_token(Tokenizer, StringFormat) ->
    next_token_nif(Tokenizer, string_format_internal(StringFormat)).

next_token(Tokenizer) ->
    next_token_nif(Tokenizer).

next_token_nif(_Tokenizer) ->
    nif_only().

next_token_nif(_Tokenizer, _InternalStringFormat) ->
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
get_position(_Tokenizer) ->
    nif_only().

-spec get_token_position(jeysn_tokenizer()) -> position().
%% @doc Return the position of the latest returned token.
get_token_position(_Tokenizer) ->
    nif_only().

-spec get_remaining_data(jeysn_tokenizer()) -> binary() | 'eof'.
%% @doc Return any remaining data as a binary. If there is no data and
%% eof is set then the atom 'eof' is returned.
get_remaining_data(_Tokenizer) ->
    nif_only().

debug(_x) ->
    ok.

%% format_json_error(#json_error{} = Err) ->
%%     ErrStr = json_error_code_to_string(Err),
%%     io_lib:format("~s:~w:~w: ~s: ~s",
%%                   [Err#json_error.

%% ------------------------------------------------------------------------

-spec encode_string(iodata()) -> binary().
encode_string(<<>>) ->
    <<"\"\"">>;
encode_string(Str) ->
    BinStr = escape_string(Str),
    <<$", BinStr/binary, $">>.

%% Note: only characters required to be escaped are escaped. Any UTF-8
%% code points in the input string are assumed to be correct, and will
%% be returned as is.
-spec escape_string(iodata()) -> binary().
escape_string(_Str) ->
    nif_only().

%% ------------------------------------------------------------------------

nif_load() ->
    Path = filename:join(code:priv_dir(jeysn), atom_to_list(?MODULE)),
    erlang:load_nif(Path, 0).

nif_only() ->
    erlang:nif_error(not_loaded).
