%%
%% Copyright Sebastian Strollo <seb@strollo.org>
%% SPDX-License-Identifier: Apache-2.0
%%
-module(jeysn).

-export([decode/1, decode/2]).
-export([decode_file/1, decode_file/2]).
-export([decode_stream/1, decode_stream/2]).
-export([encode/1, encode/2]).

%% ------------------------------------------------------------------------

-record(ds, {
          tokenizer
          , filename = <<"-">>
          , string = binary
          , name = binary % existing_atom
          , object = map % list
          , more = fun () -> eof end
         }).


decode(Str) ->
    decode(Str, []).
decode(Str, Opts) ->
    S0 = decode_opts(decode_validate_opts(Opts)),
    S = S0#ds{tokenizer = jeysn_ll:init_string(Str)},
    decode_value(S).

decode_file(FileName) ->
    decode_file(FileName, []).
decode_file(FileName, Opts0) ->
    Opts = decode_validate_opts(Opts0),
    BufSz = proplists:get_value(buffer_size, Opts, 64*1024), % 8192,
    {ok, Fd} = file:open(FileName, [read,binary,raw]),
    S0 = decode_opts(Opts),
    S = S0#ds{tokenizer = jeysn_ll:init(),
              filename = FileName,
              more = fun () -> file:read(Fd, BufSz) end},
    decode_value(S).

decode_stream(ReadFun) ->
    decode_stream(ReadFun, []).
decode_stream(ReadFun, Opts) ->
    S0 = decode_opts(decode_validate_opts(Opts)),
    S = S0#ds{tokenizer = jeysn_ll:init(),
              more = ReadFun},
    decode_value(S).

%-define(default(RECORD, ITEM), (#RECORD{})#RECORD.ITEM).
-define(default(RECORD, ITEM), element(#RECORD.ITEM, #RECORD{})).
-define(opt(NAME, REC, OPTS),
        NAME = proplists:get_value(NAME, OPTS, ?default(REC, NAME))).

decode_opts(Opts) ->
    #ds{
         ?opt(string, ds, Opts)
         , ?opt(name, ds, Opts)
         , ?opt(object, ds, Opts)
       }.

decode_validate_opts(Opts) when is_map(Opts) ->
     decode_validate_opts(proplists:from_map(Opts));
decode_validate_opts(Opts) when is_list(Opts) ->
    lists:foreach(
      fun ({name, StringType}) -> assert(string, StringType);
          ({string, StringType}) -> assert(string, StringType);
          ({object, map}) -> ok;
          ({object, list}) -> ok;
          ({buffer_size, N}) -> assert(non_zero_integer, N);
          (_) -> erlang:error(badarg)
      end,
      Opts),
    Opts;
decode_validate_opts(Opt) ->
    decode_validate_opts([Opt]).


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
            decode_object_pair(S, Token, #{})
    end;
decode_object(#ds{object = list} = S) ->
    case decode_next(S, S#ds.name) of
        '}' ->
            [{}];
        Token ->
            decode_object_pair(S, Token, [])
    end.

decode_object_pair(S, {string, Name}, Object) ->
    case decode_next(S) of
        ':' when S#ds.object == map ->
            Value = decode_value(S),
            decode_object_next(S, Object#{Name => Value});
        ':' when S#ds.object == list ->
            Value = decode_value(S),
            decode_object_next(S, [{Name, Value}|Object]);
        Other ->
            decode_error(S, Other, [':'])
    end;
decode_object_pair(S, Other, _Object) ->
    decode_error(S, Other, [string]).

decode_object_next(S, Object) ->
    case decode_next(S) of
        '}' when S#ds.object == map ->
            Object;
        '}' when S#ds.object == list andalso Object == [] ->
            [{}];
        '}' when S#ds.object == list ->
            lists:reverse(Object);
        ',' ->
            decode_object_pair(S, decode_next(S, S#ds.name), Object);
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
    decode_next(S, S#ds.string).

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
    {_, Line, Col} = jeysn_ll:get_token_position(T),
    {File, Line, Col}.

%% ------------------------------------------------------------------------

-record(es, {
          space = 0
          , indent = 0
          , list_may_be_string = false
          , io = undefined
         }).

encode(Term) ->
    encode(Term, []).

encode(Term, Opts) ->
    S = encode_opts(encode_validate_opts(Opts)),
    encode_value(Term, <<>>, 0, S).

encode_opts(Opts) ->
    #es{
         ?opt(space, es, Opts)
         , ?opt(indent, es, Opts)
         , ?opt(io, es, Opts)
         , list_may_be_string = proplists:get_bool(list_may_be_string, Opts)
       }.

encode_validate_opts(Opts) when is_map(Opts) ->
    encode_validate_opts(proplists:from_map(Opts));
encode_validate_opts(Opts0) when is_list(Opts0) ->
    Opts =
        proplists:normalize(
          Opts0,
          [{expand, [{{space,true}, [{space,1}]}
                     , {{indent, true}, [{indent, 1}]}
                     , {pretty, [{space, 1}, {indent, 2}]}
                    ]}]),
    lists:foreach(
      fun ({space, N}) -> assert(integer, N);
          ({indent, N}) -> assert(integer, N);
          ({list_may_be_string, B}) -> assert(boolen, B);
          ({io, F}) -> assert({function, 1}, F);
          (_) -> erlang:error(badarg)
      end,
      Opts),
    Opts;
encode_validate_opts(Opt) ->
    encode_validate_opts([Opt]).

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
%% Object
encode_value(Object, Out, _L, S) when map_size(Object) =:= 0 ->
    emit(<<"{}">>, Out, S);
encode_value([{}], Out, _L, S) ->
    emit(<<"{}">>, Out, S);
encode_value(Object, Out, L, S) when is_map(Object) ->
    encode_object(Object, Out, L, S);
encode_value(Object, Out, L, S) when is_tuple(hd(Object)) ->
    encode_object(Object, Out, L, S);
%% Array
encode_value([], Out, _L, S) ->
    emit(<<"[]">>, Out, S);
encode_value(Array, Out, L, #es{list_may_be_string = false} = S)
  when is_list(Array) ->
    encode_array(Array, Out, L, S);
encode_value(Value, Out, L, #es{list_may_be_string = false} = S)
  when is_list(Value) ->
    case is_string(Value) of
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
    F(Str).

is_string([]) ->
    true;
is_string([Char|Chars]) when is_integer(Char)
                             andalso (Char < 127)
                             andalso (Char > 31) ->
    is_string(Chars);
is_string(_) ->
    false.

%% ------------------------------------------------------------------------

assert(string, 'binary') -> ok;
assert(string, 'string') -> ok;
assert(string, 'atom') -> ok;
assert(string, 'existing_atom') -> ok;
assert(non_zero_integer, N) when is_integer(N) andalso N > 0 -> ok;
assert(integer, N) when is_integer(N) andalso N >= 0 -> ok;
assert(boolen, true) -> ok;
assert(boolen, false) -> ok;
assert({function, Arity}, F) when is_function(F, Arity) -> ok;
assert(_, _) ->
    erlang:error(badarg).
