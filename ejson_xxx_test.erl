-module(ejson_xxx_test).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

test() ->
    case proper:module(?MODULE, [{numtests,1000}]) of
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
-type json_number() :: 0..100000.

-type json_string() :: json_string_chars().
-type json_string_chars() :: [json_string_char(),...].
-type json_string_char() :: 48..57|65..90|97..122.  %% [0-9a-zA-Z]

-type json_array() :: {'array', [json_value()]}.

-type json_object() :: {'struct', [json_object_member()]}.
-type json_object_member() :: {json_string(), json_value()}.



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
                Term = ejson:xxx(Str),
%%                io:format("\n\nStr: ~s\nTerm: ~9999p\n\n", [Str, Term]),
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

