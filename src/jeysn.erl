%%
%% Copyright Sebastian Strollo <seb@strollo.org>
%% SPDX-License-Identifier: Apache-2.0
%%
-module(jeysn).

-export([decode/1, decode/2]).
-export([decode_file/1, decode_file/2]).
-export([decode_io/0, decode_io/1, decode_io/2]).

-export([encode/1, encode/2]).
-export([encode_file/2, encode_file/3]).
-export([encode_io/1, encode_io/2, encode_io/3]).

-export([format_error/1]).

%% ------------------------------------------------------------------------

-type decode_options() :: [decode_option()] | decode_option() | map().
%% Options to the decode functions. When given as a map they will
%% first be converted to a property list.

-type decode_option() :: atom() | {atom(), term()}.
%% TODO: document all decode options

-type read_fun() ::
        fun (() ->
                 {'ok', iodata()}
                 | eof
                 | {'error', any()}
            ).
%% A function that reads more data when needed by the JSON decoder.

-type encode_options() :: [encode_option()] | encode_option() | map().
%% Options to the encode functions. When given as a map they will
%% first be converted to a property list.

-type encode_option() :: atom() | {atom(), term()}.
%% TODO: document all decode options

-type write_fun() :: write_fun1() | write_fun2().

-type write_fun1() :: fun ((iodata()) -> any()).
%% Function that should write data emitted by the encoder.

-type write_fun2() :: fun ((iodata(), term()) -> term()).
%% Function that should write data emitted by the encoder. The second
%% argument is the term that write_fun2() returned on the previous
%% invocation.

-type json_term() :: any().
%% An Erlang term representing a JSON value.
%% TODO: better spec

-type jeysn_error() ::
        {'jeysn',
         FileName     :: binary(),
         PositionInfo :: jeysn_ll:position_info(),
         Error        :: term()
        }.

%% ------------------------------------------------------------------------
-record(ds, {
          tokenizer
          , filename = <<"-">>
          , string = binary
          , name = binary % existing_atom
          , object = map % list | {list, Wrap}
          , empty_object = [{}]
          , wrap_array = undefined % Term
          , more = fun () -> eof end
          , trailing_data = error        % 'error', 'return', 'ignore'
         }).


-spec decode(String :: iodata()) ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', jeysn_error()}.
%% @equiv decode(String, [])
decode(String) ->
    decode(String, []).

-spec decode(String :: iodata(), Options :: decode_options()) ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', jeysn_error()}.
%% @doc Decode a JSON value and return its Erlang representation.
decode(String, Options) ->
    S0 = decode_opts(decode_validate_opts(Options)),
    S = S0#ds{tokenizer =
                  jeysn_ll:init_string(String, [{string, S0#ds.string}])},
    decode_value_0(S).

-spec decode_file(Filename :: file:name_all()) ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', Reason} when
      Reason :: jeysn_error() | file:posix() | 'badarg' | 'system_limit'.
%% @equiv decode_file(ReadFun, [])
decode_file(FileName) ->
    decode_file(FileName, []).

-spec decode_file(Filename :: file:name_all(), Options :: decode_options()) ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', Reason} when
      Reason :: jeysn_error() | file:posix() | 'badarg' | 'system_limit'.
%% @doc Decode a JSON value in file FileName and return its Erlang
%% representation.
decode_file(FileName, Options) ->
    Opts = decode_validate_opts(Options),
    BufSz = proplists:get_value(buffer_size, Opts, 64*1024), % 8192,
    case file:open(FileName, [read,binary,raw]) of
        {ok, Fd} ->
            try
                S0 = decode_opts(Opts),
                S = S0#ds{tokenizer = jeysn_ll:init([{string, S0#ds.string}]),
                          filename = FileName,
                          more = fun () -> file:read(Fd, BufSz) end},
                decode_value_0(S)
            after
                file:close(Fd)
            end;
        {error, _Reson} = Error ->
            Error
    end.

-spec decode_io() ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', Reason} when
      Reason :: jeysn_error() | term().
%% @doc prompt for text and parse it as a JSON object
decode_io() ->
    decode_io(
      fun () ->
              case io:get_line("jeysn> ") of
                  "\n" -> eof;
                  eof -> eof;
                  {error, _Err} = Error -> Error;
                  String -> {ok, String}
              end
      end, []).

-spec decode_io(ReadFun :: read_fun()) ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', Reason} when
      Reason :: jeysn_error() | term().
%% @equiv decode_io(ReadFun, [])
decode_io(ReadFun) ->
    decode_io(ReadFun, []).

-spec decode_io(ReadFun :: read_fun(), Options :: decode_options()) ->
          {'ok', json_term()}
              | {'ok', json_term(), RemainingData :: binary()}
              | {'error', Reason} when
      Reason :: jeysn_error() | term().
%% @doc Decode a JSON value that is returned by (repeatedly) invoking
%% ReadFun() and return its Erlang representation.
decode_io(ReadFun, Options) ->
    S0 = decode_opts(decode_validate_opts(Options)),
    S = S0#ds{tokenizer = jeysn_ll:init([{string, S0#ds.string}]),
              more = ReadFun},
    decode_value_0(S).

%% ------------------------------------------------------------------------

%-define(default(RECORD, ITEM), (#RECORD{})#RECORD.ITEM).
-define(default(RECORD, ITEM), element(#RECORD.ITEM, #RECORD{})).
-define(opt(NAME, REC, OPTS),
        NAME = proplists:get_value(NAME, OPTS, ?default(REC, NAME))).
-define(opt_b(NAME, OPTS),
        NAME = proplists:get_bool(NAME, OPTS)).

decode_opts(Opts) ->
    #ds{
         ?opt(string, ds, Opts)
         , ?opt(name, ds, Opts)
         , ?opt(object, ds, Opts)
         , ?opt(empty_object, ds, Opts)
         , ?opt(wrap_array, ds, Opts)
         , ?opt(trailing_data, ds, Opts)
       }.

decode_validate_opts(Opts) when is_map(Opts) ->
     decode_validate_opts(proplists:from_map(Opts));
decode_validate_opts(Opts0) when is_list(Opts0) ->
    Opts =
        proplists:normalize(
          Opts0,
          [{expand, [{{json2, true}, [{object, {list, struct}},
                                      {empty_object, []},
                                      {wrap_array, array},
                                      {string, string},
                                      {name, string}]}
                     , {{object, proplist},
                        [{object, list}, {empty_object, [{}]}]}
                     , {{object, tuple},
                        [{object, {list, {}}}, {empty_object, []}]}
                    ]}]),
    lists:foreach(
      fun ({name, StringType}) -> assert(name, string, StringType);
          ({string, StringType}) -> assert(string, string, StringType);
          ({buffer_size, N}) -> assert(buffer_size, {integer, 0}, N);
          (_) -> ok
      end,
      Opts),
    Opts;
decode_validate_opts(Opt) ->
    decode_validate_opts([Opt]).

%% ------------------------------------------------------------------------

decode_value_0(S) ->
    case decode_value(S) of
        {error, _} = Error ->
            Error;
        Value ->
            decode_trailing_data(S, Value)
    end.

decode_trailing_data(#ds{trailing_data = ignore}, Value) ->
    {ok, Value};
decode_trailing_data(#ds{trailing_data = return} = S, Value) ->
    {ok, Value, jeysn_ll:get_remaining_data(S#ds.tokenizer)};
decode_trailing_data(#ds{trailing_data = error} = S, Value) ->
    case jeysn_ll:get_remaining_data(S#ds.tokenizer) of
        eof ->
            {ok, Value};
        <<>> ->
            %% There is no more non-whitespace data, but the read
            %% function has not yet returned eof, continue invoking it
            %% until we either get some data or eof
            case decode_more(S) of
                ok ->
                    decode_trailing_data(S, Value);
                Error ->
                    Error
            end;
        Data ->
            PositionInfo = jeysn_ll:get_position_info(S#ds.tokenizer),
            mk_jeysn_error(S#ds.filename, PositionInfo, {trailing_data, Data})
    end.

%% ------------------------------------------------------------------------

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
decode_object(#ds{object = map} = S) ->
    case decode_next(S, S#ds.name) of
        '}' ->
            #{};
        Token ->
            decode_object_pair(S, Token, [])
    end;
decode_object(#ds{object = list} = S) ->
    case decode_next(S, S#ds.name) of
        '}' ->
            S#ds.empty_object;
        Token ->
            decode_object_pair(S, Token, [])
    end;
decode_object(#ds{object = {list, Wrap}} = S) ->
    case decode_next(S, S#ds.name) of
        '}' when Wrap == {} ->
            {S#ds.empty_object};
        '}' ->
            {Wrap, S#ds.empty_object};
        Token ->
            decode_object_pair(S, Token, [])
    end.

decode_object_pair(S, {string, Name}, Object) ->
    case decode_next(S) of
        ':' ->
            case decode_value(S) of
                {error, _} = Error ->
                    Error;
                Value ->
                    decode_object_next(S, [{Name, Value}|Object])
            end;
        Other ->
            decode_error(S, Other, [':'])
    end;
decode_object_pair(S, Other, _Object) ->
    decode_error(S, Other, [string]).

decode_object_next(S, Object) ->
    case decode_next(S) of
        '}' when S#ds.object == map ->
            maps:from_list(Object);
        '}' when S#ds.object == list ->
            lists:reverse(Object);
        '}' when element(1, S#ds.object) == list andalso
                 element(2, S#ds.object) == {} ->
            {lists:reverse(Object)};
        '}' when element(1, S#ds.object) == list ->
            {element(2, S#ds.object), lists:reverse(Object)};
        ',' ->
            decode_object_pair(S, decode_next(S, S#ds.name), Object);
        Other ->
            decode_error(S, Other, ['}', ','])
    end.

decode_array(S) ->
    case decode_next(S) of
        ']' when S#ds.wrap_array == undefined ->
            [];
        ']' ->
            {S#ds.wrap_array, []};
        Token ->
            case decode_value(S, Token) of
                {error, _} = Error ->
                    Error;
                Value ->
                    decode_array_next(S, [Value])
            end
    end.

decode_array_next(S, Array) ->
    case decode_next(S) of
        ']' when S#ds.wrap_array == undefined ->
            lists:reverse(Array);
        ']' ->
            {S#ds.wrap_array, lists:reverse(Array)};
        ',' ->
            case decode_value(S) of
                {error, _} = Error ->
                    Error;
                Value ->
                    decode_array_next(S, [Value|Array])
            end;
        Other ->
            decode_error(S, Other, [']', ','])
    end.

decode_next(S) ->
    decode_next(S, S#ds.string).

decode_next(S, StrFmt) ->
    case jeysn_ll:next_token(S#ds.tokenizer, StrFmt) of
        more ->
            case decode_more(S) of
                ok ->
                    decode_next(S, StrFmt);
                {error, _} = Error ->
                    Error
            end;
        {number, IntegerBinary} ->
            binary_to_integer(IntegerBinary);
        TokenOrErrorOrEOF ->
            TokenOrErrorOrEOF
    end.

decode_more(S) ->
    case (S#ds.more)() of
        {ok, Data} ->
            jeysn_ll:data(S#ds.tokenizer, Data);
        eof ->
            jeysn_ll:eof(S#ds.tokenizer);
        {error, Error} ->
            PositionInfo = jeysn_ll:get_position_info(S#ds.tokenizer),
            mk_jeysn_error(S#ds.filename, PositionInfo, {more, Error})
    end.

-spec decode_error(#ds{}, TokenOrError, Expected) ->
          {'error', jeysn_error()} when
      TokenOrError :: jeysn_ll:json_token() | jeysn_ll:json_token_error(),
      Expected     :: [ atom() ].


decode_error(#ds{filename = File}, {error, InfoStr, PosInfo}, _Expected) ->
    mk_jeysn_error(File, PosInfo, {scan, InfoStr});
decode_error(#ds{tokenizer = T, filename = File}, Token, Expected) ->
    Position = jeysn_ll:get_token_position(T),
    PositionInfo = {Position, <<>>, <<>>},
    Error = {unexpected, Token, Expected},
    mk_jeysn_error(File, PositionInfo, Error).

mk_jeysn_error(FileName, PositionInfo, Error) ->
    {error, {jeysn, FileName, PositionInfo, Error}}.

%% ------------------------------------------------------------------------

format_error({error, Error}) ->
    format_error(Error);
format_error({jeysn, FileName, PositionInfo, Error}) ->
    iolist_to_binary(
      [format_postion(FileName, PositionInfo),
       " Error: ",
       format_jeysn_error(Error)]);
format_error(Atom) when is_atom(Atom) ->
    file:format_error(Atom).

format_postion(FileName, {{_Offset, Line, Col}, _Before, _After}) ->
    io_lib:format("~s:~w:~w:", [FileName, Line, Col]).

format_jeysn_error({unexpected, What, Expected}) ->
    io_lib:format("got ~0p expected ~w", [What, Expected]);
format_jeysn_error({scan, String}) ->
    io_lib:format("while scanning: ~s", [String]);
format_jeysn_error({more, Error}) ->
    io_lib:format("while trying to get more bytes: ~s", [format_error(Error)]);
format_jeysn_error({trailing_data, _Data}) ->
    "trailing data after JSON term";
format_jeysn_error(Err) ->
    io_lib:format("~0p", [Err]).

%% ------------------------------------------------------------------------

-record(es, {
          space = 0
          , indent = 0
          , nl = false
          , list_may_be_string = false
          , records = undefined
          , record_undefined = null
          , io = undefined
         }).

-spec encode(json_term()) -> iodata().
%% @equiv encode(Term, [])
encode(Term) ->
    encode(Term, []).

-spec encode(json_term(), encode_options()) -> any().
%% @doc Encode an Erlang term to its JSON representation and by
%% default return it as a binary().
encode(Term, Options) ->
    S = encode_opts(encode_validate_opts(Options)),
    encode_value_0(Term, <<>>, S).

-spec encode_file(json_term(), file:name_all()) -> 'ok'.
%% @equiv encode_file(Term, FileName, [])
encode_file(Term, FileName) ->
    encode_file(Term, FileName, []).

-spec encode_file(json_term(), file:name_all(), encode_options()) -> 'ok'.
%% @doc Encode an Erlang term to its JSON representation and write it
%% to the file FileName.
encode_file(Term, FileName, Options) ->
    {ok, Fd} = file:open(FileName, [raw,binary,write]),
    try
        WriteF = fun (Data) -> ok = file:write(Fd, Data) end,
        encode(Term, [{io, WriteF}|Options])
    after
        file:close(Fd)
    end.

-spec encode_io(json_term()) -> 'ok'.
%% @doc Encode an Erlang term to its JSON representation and write it
%% to standard out.
encode_io(Term) ->
    encode_io(Term, standard_io, []).

-spec encode_io(json_term(), encode_options()) -> 'ok'.
%% @doc Encode an Erlang term to its JSON representation and write it
%% to standard out.
encode_io(Term, Options) ->
    encode_io(Term, standard_io, Options).

-spec encode_io(json_term(),
                io:device() | write_fun(),
                encode_options()) ->
                       any().
%% @doc Encode an Erlang term to its JSON representation and write it
%% to specified io device, or by invoking the supplied write function.
encode_io(Term, standard_io, Options) when is_list(Options) ->
    encode(Term, [{io, fun io:put_chars/1}|Options]);
encode_io(Term, WriteF, Options) when is_list(Options) andalso
                                      is_function(WriteF, 1) ->
    encode(Term, [{io, WriteF}|Options]);
encode_io(Term, WriteF, Options) when is_list(Options) andalso
                                      is_function(WriteF, 2) ->
    encode(Term, [{io, WriteF}|Options]);
encode_io(Term, IoDevice, Options) when is_list(Options) ->
    encode(Term,
           [{io, fun (Data) -> io:put_chars(IoDevice, Data) end} | Options]);
encode_io(Term, Io, Option) ->
    encode_io(Term, Io, [Option]).


%% ------------------------------------------------------------------------

encode_opts(Opts) ->
    #es{
         ?opt(space, es, Opts)
         , ?opt(indent, es, Opts)
         , ?opt_b(nl, Opts)
         , ?opt(io, es, Opts)
         , ?opt_b(list_may_be_string, Opts)
         , ?opt(records, es, Opts)
         , ?opt(record_undefined, es, Opts)
       }.

encode_validate_opts(Opts) when is_map(Opts) ->
    encode_validate_opts(proplists:from_map(Opts));
encode_validate_opts(Opts0) when is_list(Opts0) ->
    Opts =
        proplists:normalize(
          Opts0,
          [{expand, [{{space,true}, [{space,1}]}
                     , {{indent, true}, [{indent, 1}]}
                     , {pretty, [{space, 1}, {indent, 2}, {nl, true}]}
                    ]}]),
    lists:foreach(
      fun ({space, N}) -> assert(space, integer, N);
          ({indent, N}) -> assert(indent, integer, N);
          ({nl, B}) -> assert(nl, boolean, B);
          ({list_may_be_string, B}) -> assert(list_may_be_string, boolean, B);
          ({records, M}) when is_map(M) -> ok;
          ({record_undefined, _}) -> ok;
          ({io, F}) -> assert(io, {'or', {function, 1}, {function, 2}}, F);
          (nl) -> ok;
          (list_may_be_string) -> ok;
          (_Opt) -> erlang:error(badarg, [_Opt])
      end,
      Opts),
    Opts;
encode_validate_opts(Opt) ->
    encode_validate_opts([Opt]).

%% ------------------------------------------------------------------------

encode_value_0(Term, Out0, S) ->
    Out1 = encode_value(Term, Out0, 0, S),
    Out2 =
        case S of
            #es{nl = true, indent = N} when N > 0 ->
                emit(<<$\n>>, Out1, S);
            _ ->
                Out1
        end,
    %% emit(eoj, Out2, S);
    Out2.

encode_value(false, Out, _L, S) -> emit(<<"false">>, Out, S);
encode_value(null,  Out, _L, S) -> emit(<<"null">>, Out, S);
encode_value(true,  Out, _L, S) -> emit(<<"true">>, Out, S);
encode_value(N, Out, _L, S) when is_integer(N) ->
    emit(integer_to_binary(N), Out, S);
encode_value(N, Out, _L, S) when is_float(N) ->
    emit(list_to_binary(io_lib:write(N)), Out, S);
%% String
encode_value(Str, Out, _L, S) when is_atom(Str) ->
    encode_string(Str, Out, S);
encode_value(Str, Out, _L, S) when is_binary(Str) ->
    encode_string(Str, Out, S);
encode_value({string, Str}, Out, _L, S) ->
    encode_string(Str, Out, S);
%% Object
encode_value([{}], Out, _L, S) ->
    emit(<<"{}">>, Out, S);
encode_value({}, Out, _L, S) ->
    emit(<<"{}">>, Out, S);
encode_value({Object}, Out, L, S) when is_list(Object) ->
    encode_object(Object, Out, L, S);
encode_value({struct, Object}, Out, L, S) ->
    encode_object(Object, Out, L, S);
encode_value({object, Object}, Out, L, S) ->
    encode_object(Object, Out, L, S);
encode_value(Object, Out, L, S) when is_map(Object) ->
    encode_object(Object, Out, L, S);
encode_value(Object, Out, L, S) when tuple_size(hd(Object)) =:= 2 ->
    encode_object(Object, Out, L, S);
encode_value(Record, Out, L, #es{records = Records} = S)
  when is_map_key(element(1, Record), Records) ->
    [RecordName|Values] = tuple_to_list(Record),
    Object = record_to_proplist(
               maps:get(RecordName, Records), Values, S#es.record_undefined),
    encode_object(Object, Out, L, S);
%% Array
encode_value([], Out, _L, S) ->
    emit(<<"[]">>, Out, S);
encode_value({array, []}, Out, _L, S) ->
    emit(<<"[]">>, Out, S);
encode_value({array, Array}, Out, L, S) ->
    encode_array(Array, Out, L, S);
encode_value(Array, Out, L, #es{list_may_be_string = false} = S)
  when is_list(Array) ->
    encode_array(Array, Out, L, S);
encode_value(Value, Out, L, #es{list_may_be_string = true} = S)
  when is_list(Value) ->
    case io_lib:printable_list(Value) of
        false ->
            encode_array(Value, Out, L, S);
        true ->
            encode_string(Value, Out, S)
    end.

encode_string(Str, Out, S) when is_binary(Str) ->
    emit(jeysn_ll:encode_string(Str), Out, S);
encode_string(Str, Out, S) when is_atom(Str) ->
    emit(jeysn_ll:encode_string(atom_to_binary(Str, utf8)), Out, S);
encode_string(N, Out, S) when is_integer(N) ->
    Str = integer_to_binary(N),
    emit(jeysn_ll:encode_string(Str), Out, S);
encode_string(Str, Out, S) ->
    emit(jeysn_ll:encode_string(Str), Out, S).

encode_array(Array, Out, L, S) ->
    Out1 = emit(encode_indent(<<"[">>, S, L+1), Out, S),
    Out2 = encode_array_1(Array, Out1, L+1, S),
    Out3 = emit(encode_close_indent(<<"]">>, S, L), Out2, S),
    Out3.

encode_array_1([Value], Out, L, S) ->
    encode_value(Value, Out, L, S);
encode_array_1([Value|Values], Out, L, S) ->
    Out1 = encode_value(Value, Out, L, S),
    Out2 = emit(encode_indent_or_space(<<",">>, S, L), Out1, S),
    encode_array_1(Values, Out2, L, S).

encode_object([], Out, _L, S) ->
    emit(<<"{}">>, Out, S);
encode_object(Object, Out, _L, S) when map_size(Object) =:= 0 ->
    emit(<<"{}">>, Out, S);
encode_object(Object, Out, L, S) ->
    Out1 = emit(encode_indent(<<"{">>, S, L+1), Out, S),
    Out2 =
        if
            is_map(Object) ->
                First = maps:next(maps:iterator(Object)),
                encode_object_map(First, Out1, L+1, S);
            true ->
                encode_object_list(Object, Out1, L+1, S)
        end,
    Out3 = emit(encode_close_indent(<<"}">>, S, L), Out2, S),
    Out3.

encode_object_map({Name, Value, I}, Out, L, S) ->
    Out1 = encode_object_pair(Name, Value, Out, L, S),
    case maps:next(I) of
        none ->
            Out1;
        Next ->
            Out2 = emit(encode_indent_or_space(<<",">>, S, L), Out1, S),
            encode_object_map(Next, Out2, L, S)
    end.

encode_object_list([{Name, Value}], Out, L, S) ->
    encode_object_pair(Name, Value, Out, L, S);
encode_object_list([{Name, Value}|Pairs], Out, L, S) ->
    Out1 = encode_object_pair(Name, Value, Out, L, S),
    Out2 = emit(encode_indent_or_space(<<",">>, S, L), Out1, S),
    encode_object_list(Pairs, Out2, L, S);
encode_object_list([Name|Pairs], Out, L, S) ->
    encode_object_list([{Name, true}|Pairs], Out, L, S).

encode_object_pair(Name, Value, Out, L, S) ->
    Out1 = encode_string(Name, Out, S),
    Out2 = emit(encode_space(<<$:>>, S), Out1, S),
    encode_value(Value, Out2, L, S).

encode_indent(Str, #es{indent = 0}, _Lvl) -> Str;
encode_indent(Str, #es{indent = N}, Lvl) -> [Str, $\n | space(N * Lvl)].

encode_close_indent(Str, #es{indent = 0}, _Lvl) -> Str;
encode_close_indent(Str, #es{indent = N}, Lvl) -> [$\n, space(N * Lvl), Str].

encode_indent_or_space(Str, #es{indent = 0, space = 0}, _Lvl) ->
    Str;
encode_indent_or_space(Str, #es{indent = N}, Lvl) when N > 0 ->
    [Str, $\n | space(N * Lvl)];
encode_indent_or_space(Str, #es{space = N}, _Lvl) ->
    [Str, space(N)].

encode_space(Str, #es{space = N}) ->
    [Str, space(N)].

space(0) -> "";
space(1) -> " ";
space(2) -> "  ";
space(3) -> "   ";
space(4) -> "    ";
space(6) -> "      ";
space(8) -> "        ";
space(N) when is_integer(N) andalso N > 4 ->
    lists:duplicate(N, $\040).


emit(Str, Out, #es{io = undefined}) when is_binary(Str) ->
    <<Out/binary, Str/binary>>;
emit(Str, Out, #es{io = undefined}) ->
    BinStr = iolist_to_binary(Str),
    <<Out/binary, BinStr/binary>>;
emit(Str, _Out, #es{io = F}) when is_function(F, 1) ->
    F(Str);
emit(Str, Out, #es{io = F}) when is_function(F, 2) ->
    F(Str, Out).

record_to_proplist([_Name|Names], [undefined|Values], remove = Undefined) ->
    record_to_proplist(Names, Values, Undefined);
record_to_proplist([Name|Names], [undefined|Values], Undefined) ->
    [{Name, Undefined} | record_to_proplist(Names, Values, Undefined)];
record_to_proplist([Name|Names], [Value|Values], Undefined) ->
    [{Name, Value} | record_to_proplist(Names, Values, Undefined)];
record_to_proplist([], [], _) ->
    [].

%% ------------------------------------------------------------------------

assert(Name, Type, Value) ->
    case assert(Type, Value) of
        true ->
            ok;
        false ->
            erlang:error(badarg, [Name, Value])
    end.

assert(string, 'binary') -> true;
assert(string, 'string') -> true;
assert(string, 'atom') -> true;
assert(string, 'existing_atom') -> true;
assert({integer, LgT}, N) when is_integer(N) andalso N > LgT -> true;
%assert({integer, From, To}, N) when is_integer(N)
%                                    andalso N >= From
%                                    andalso N =< To -> true;
assert(integer, N) when is_integer(N) andalso N >= 0 -> true;
assert(boolean, true) -> true;
assert(boolean, false) -> true;
assert({function, Arity}, F) when is_function(F, Arity) -> true;
assert({'or', A, B}, Value) ->
    assert(A, Value) orelse assert(B, Value);
%assert({'and', A, B}, Value) ->
%    assert(A, Value) andalso assert(B, Value);
assert(_, _) ->
    false.
