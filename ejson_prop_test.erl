-module(ejson_prop_test).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

prop_parse() ->
    ?FORALL(String, json_string_chars(), begin ejson:xxx([$",String,$"]), true end).


%% -type json_value() :: json_false() | json_null() | json_true() |
%%                       json_number() | json_string() |
%%                       json_array().
%%                      json_array() | json_object().

%% -type json_false() :: "false".
%% -type json_null()  :: "null".
%% -type json_true()  :: "true".
%% -type json_string() :: [$", json_string_chars(), $"].

-type json_string_chars() :: [json_string_char(),...].
-type json_string_char() :: 97..122.  %% $a..$z.
-type json_ws_char() :: 16#20 | 16#09 | 16#0a | 16#0d.
-type json_ws()      :: list(json_ws_char()).

%%-type json_array() :: ["[", json_array_items(), "]"].
%%-type json_array_items() :: json_value() | [json_value(),",",json_value()].
%%-type json_array_items() :: json_value() | json_array_items2().

%% -type json_object() :: ["{", json_object_items(), "}"].
%% -type json_object_items() :: json_object_item() |
%%                              [json_object_item(), ",", json_object_item()].
%% -type json_object_item() :: [json_string(), ":", json_value()].

json_number() ->
    integer_to_list(integer()).

%%json_string() -> [json_ws(), json_value(), json_ws()].
%%json_string() -> [json_ws(), json_string(), json_ws()].

json_false() -> "false".
json_null()  -> "null".
json_true()  -> "true".

json_string() -> [$", json_string_chars(), $"].

%%json_array() -> ["[", json_array_items(), "]"].
%%json_array_items2() -> [json_value(),",",json_value()].
