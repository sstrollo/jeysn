-module(ejson_xxx_test).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

test() ->
    proper:module(?MODULE).


-type json_value() :: 'false' | 'null' | 'true' | json_number() |
                      json_string() | json_array() | json_object().

%-type json_number() :: integer().
-type json_number() :: 0..100000.

-type json_string() :: json_string_chars().
-type json_string_chars() :: [json_string_char(),...].
-type json_string_char() :: 32|48..57|65..90|97..122.  %% [0-9a-zA-Z]

-type json_array() :: {'array', [json_value()]}.

-type json_object() :: {'struct', [json_object_member()]}.
-type json_object_member() :: {json_string(), json_value()}.



-type json_ws_char() :: 16#20 | 16#09 | 16#0a | 16#0d.
-type json_ws()      :: [json_ws_char()].

prop_decode() ->
    ?FORALL(V, json_value(), ejson:xxx(encode(V)) == V).

%json_string() ->
%    ?LET(X, json_value(), lists:flatten(encode(X))).

%% json_ws() ->
%%     ?LET(X, json_ws(), X).

json_ws() -> 16#20.

encode_value(JSV) ->
    [json_ws(),encode(JSV),json_ws()].

encode(false) -> "false";
encode(null) -> "null";
encode(true) -> "true";
encode(N) when is_integer(N) -> integer_to_list(N);
encode(N) when is_float(N) -> float_to_list(N);
encode(Str) when is_list(Str) -> [$",Str,$"];
encode({'array', Array}) ->
    [$[,encode_array_items(Array),$]];
encode({'struct', Members}) ->
    [${,encode_object_members(Members),$}].

encode_array_items([]) ->
    "";
encode_array_items([Item]) ->
    encode_value(Item);
encode_array_items([Item|Items]) ->
    [encode_value(Item),json_ws(),$,,json_ws(),encode_array_items(Items)].

encode_object_members([]) ->
    "";
encode_object_members([{Key,Value}]) ->
    [encode_value(Key),json_ws(),$:,encode_value(Value)];
encode_object_members([{Key,Value}|Members]) ->
    [encode_value(Key),json_ws(),$:,encode_value(Value),$,,
     encode_object_members(Members)].

