-module(ejson_prop_test).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

test() ->
    case proper:module(?MODULE, [{numtests,2000}]) of
        [] ->
            erlang:halt(0);
        Res ->
            io:format("~p\n", [Res]),
            erlang:halt(1)
    end.

s() ->
    proper:check_specs(ejson).

-type json_value() :: 'false' | 'null' | 'true' | json_number() |
                      json_string() | json_array() | json_object().

%-type json_number() :: integer().
-type json_number() :: number().

-type json_string() :: json_string_chars().
-type json_string_chars() :: [json_string_char(),...].
-type json_string_char() :: 32|48..57|65..90|97..122.  %% [ 0-9a-zA-Z]

-type json_array() :: {'array', [json_value()]}.

-type json_object() :: {'struct', [json_object_member()]}.
-type json_object_member() :: {json_string(), json_value()}.


prop_number() ->
    ?FORALL(Ns, list(json_number()),
            begin
                T = ejson:init_string([[number_to_string(N),$ ]||N <- Ns]),
                same_numbers(T, Ns)
            end).

number_to_string(N) when is_integer(N) -> integer_to_list(N);
number_to_string(N) when is_float(N) -> float_to_list(N).

same_numbers(T, []) ->
    eof == ejson:next_token(T);
same_numbers(T, [N|Ns]) ->
    case same_number(N, ejson:next_token(T)) of
        true ->
            same_numbers(T, Ns);
        false ->
            false
    end.

same_number(N, {number, N}) -> true;
same_number(N, N) -> true;
same_number(N, {number, Str}) when is_list(Str) ->
    N == list_to_integer(Str);
same_number(N1, N2) ->
    io:format("~p != ~p\n", [N1, N2]),
    false.

%% Basic Multilingual Plane
unicode_bmp() ->
    oneof([range(0,16#D7FF),range(16#E000,16#FFFF)]).

%% Supplementary Planes
unicode_sp() ->
    range(16#10000, 16#10FFFF).

valid_utf8_codepoint() ->
    oneof([unicode_bmp(), unicode_sp()]).

prop_utf8_escaped_bmp() ->
    ?FORALL(X, unicode_bmp(), verify_escape_u_encoding(X)).

prop_utf8_escaped_sp() ->
    ?FORALL(X, unicode_sp(), verify_escape_u_encoding(X)).

verify_escape_u_encoding(X) ->
    Str1 = json_escape_u(X),
    Str2 = json_escape_l(X),
    Bin = <<X/utf8,X/utf8>>,
%%    io:format("\n~s\n", [[Str1,Str2]]),
    T = ejson:init_string([$", Str1, Str2, $"]),
    {string, Bin} == ejson:next_token(T)
        andalso eof == ejson:next_token(T).

json_escape_u(CodePoint) when CodePoint =< 16#ffff ->
    io_lib:format("\\u~4.16.0B", [CodePoint]);
json_escape_u(CodePoint) ->
    <<N1:16/unsigned, N2:16/unsigned>> = <<CodePoint/utf16>>,
    [json_escape_u(N1),json_escape_u(N2)].

json_escape_l(CodePoint) when CodePoint =< 16#ffff ->
    io_lib:format("\\u~4.16.0b", [CodePoint]);
json_escape_l(CodePoint) ->
    <<N1:16/unsigned, N2:16/unsigned>> = <<CodePoint/utf16>>,
    [json_escape_l(N1),json_escape_l(N2)].


invalid_escape_u_sequence() ->
    oneof([escape_u_without_hex(),
           escape_u_with_invalid_cp_followed_by_non_escape_u()]).

escape_u_with_invalid_cp_followed_by_non_escape_u() ->
    ?LET(X, oneof([range(16#DC00,16#DFFF), range(16#D800,16#DBFF)]),
         begin
%%             io:format("\nEEE ~s\n", [json_escape_u(X)]),
             lists:flatten([json_escape_u(X),
                            non_empty(list(json_string_char_extended()))])
         end).

escape_u_without_hex() ->
    [$\\,$u|non_empty(list(non_hex_char()))].


non_hex_char() ->
    ?SUCHTHAT(X, json_string_char_extended(),
              not(lists:member(X, "0123456789abcdefABCDEF"))).

%% All printable ascii, except reverse solidus (92) and quote (34)
json_string_char_extended() ->
    ?SUCHTHAT(X, range(32,126), (X /= 34) andalso (X /= 92)).

invalid_escape_sequence() ->
    [$\\,non_escape_char()].
non_escape_char() ->
    ?SUCHTHAT(X, json_string_char_extended(), not(lists:member(X, "/bfnrtu"))).

prop_skip_invalid_escape_sequence() ->
    ?FORALL([Pre, Seq, Post],
            [json_string_chars(),invalid_escape_sequence(),json_string_chars()],
            begin
                String = lists:flatten([Pre,Seq,Post]),
                JSON_Value = [$", String, $"],
%%                io:format("\n~w ~w\n", [String, ejson:xxx(JSON_Value)]),
                String == ejson:xxx(JSON_Value)
            end).

prop_skip_invalid_escape_u_sequence() ->
    ?FORALL([Pre, Seq, Post],
            [json_string_chars(),
             invalid_escape_u_sequence(),
             json_string_chars()],
            begin
                String = lists:flatten([Pre,Seq,Post]),
                JSON_Value = [$", String, $"],
%%                io:format("\n~w ~w\n", [String, ejson:xxx(JSON_Value)]),
                String == ejson:xxx(JSON_Value)
            end).

%%-type json_ws_char() :: 16#20 | 16#09 | 16#0a | 16#0d.
%%-type json_ws()      :: [json_ws_char()].
json_ws() ->
    list(frequency([{80,16#20},{15,16#0a},{4,16#09},{1,16#0d}])).

prop_decode() ->
    ?FORALL(V, json_value(),
            begin
                Str = encode_value(V),
%%                io:format("\n\n~s\n\n", [Str]),
                ejson:xxx(Str) == V
            end).

prop_decode_ws() ->
    ?FORALL([V, Space], [json_value(), json_ws()],
            begin
                WS = fun() -> Space end,
                Str = encode_value(V, WS),
%%                io:format("\n\n~s\n\n", [Str]),
%%                io:format("\n~w\n", [Space]),
                ejson:xxx(Str) == V
            end).

test_str() ->
    union(["true", "false", "null"]).

prop_test() ->
    ?FORALL(Str, test_str(),
            begin
                _Term = ejson:xxx(Str),
%%                io:format("\n\nStr: ~s\nTerm: ~9999p\n\n", [Str, _Term]),
                true
            end).

chunk_size() ->
    range(1,8).

prop_chop() ->
    ?FORALL([V, Space, ChunkSize], [json_value(), json_ws(), chunk_size()],
            begin
                WS = fun() -> Space end,
                V1 = chopped(encode_value(V, WS), ChunkSize),
                V1 == V
            end).

chopped(String, ChunkSize) ->
    BinString = list_to_binary(String),
    Ref = make_ref(),
    put(Ref, BinString),
    ReadF =
        fun () ->
                case get(Ref) of
                    eof ->
%%                        io:format("eof\n", []),
                        eof;
                    <<Bin1:ChunkSize/binary, Bin2/binary>> ->
%%                        io:format("Bin1:\"~s\"  Bin2:\"~s\"\n",
%%                                  [Bin1,Bin2]),
                        put(Ref, Bin2),
                        {ok, Bin1};
                    Bin1 ->
%%                        io:format("Bin1:\"~s\" eof", [Bin1]),
                        put(Ref, eof),
                        {ok, Bin1}
                end
        end,
    try
        ejson:xxxrf(ReadF)
    after
        erase(Ref)
    end.

%json_string() ->
%    ?LET(X, json_value(), lists:flatten(encode(X))).

%% json_ws() ->
%%     ?LET(X, json_ws(), X).

%%json_ws() -> 16#20.

encode_value(JSV) ->
    encode_value(JSV, fun() -> "" end).
encode_value(JSV, WS) ->
    [WS(),encode(JSV, WS),WS()].


encode(false, _) -> "false";
encode(null, _) -> "null";
encode(true, _) -> "true";
encode(N, _) when is_integer(N) -> integer_to_list(N);
encode(N, _) when is_float(N) -> float_to_list(N);
encode(Str, _) when is_list(Str) -> [$",Str,$"];
encode({'array', Array}, WS) ->
    [$[,encode_array_items(Array, WS),$]];
encode({'struct', []}, _) ->
    "{}";
encode({'struct', Members}, WS) ->
    [${,$ ,encode_object_members(Members, WS),$ ,$}].

encode_array_items([], _) ->
    "";
encode_array_items([Item], WS) ->
    encode_value(Item, WS);
encode_array_items([Item|Items], WS) ->
    [encode_value(Item, WS),$,,$ ,encode_array_items(Items, WS)].

encode_object_members([], _) ->
    "";
encode_object_members([{Key,Value}], WS) ->
    [encode_value(Key, WS),$:,$ ,encode_value(Value, WS)];
encode_object_members([{Key,Value}|Members], WS) ->
    [encode_value(Key, WS),$:,$ ,encode_value(Value, WS),$,,$ ,
     encode_object_members(Members, WS)].

