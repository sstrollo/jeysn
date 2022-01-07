-module(jeysn_tests).

-include_lib("eunit/include/eunit.hrl").

files_test() ->
    JSONFiles = json_files("."),
    lists:foreach(
      fun (File) ->
              Term = jeysn:decode_file(File),
              String = iolist_to_binary(jeysn:encode(Term)),
              Term = jeysn:decode(String),
              ok
      end, JSONFiles),
    ok.

json_files(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:filter(
      fun (FileName) ->
              case lists:reverse(FileName) of
                  [$n,$o,$s,$j,$.|_] ->
                      true;
                  _ ->
                      false
              end
      end, Files).

-record(foo, {a = 42, b = false, c = null, d, e}).
-record(bar, {boo, baz, bing}).

records_test() ->
    RI = #{foo => record_info(fields, foo)},

    R = #foo{d = <<"hello">>},

    <<"{\"a\":42,\"b\":false,\"c\":null,\"d\":\"hello\",\"e\":null}">> =
        B1 = jeysn:encode(R, [{records, RI}]),

    <<"{\"a\":42,\"b\":false,\"c\":null,\"d\":\"hello\",\"e\":false}">> =
        B2 = jeysn:encode(R, [{records, RI}, {record_undefined, false}]),

    <<"{\"a\":42,\"b\":false,\"c\":null,\"d\":\"hello\"}">> =
        B3 = jeysn:encode(R, [{records, RI}, {record_undefined, remove}]),

    DOpts = [{object,list},{name,existing_atom}],

    [{a,42},{b,false},{c,null},{d,<<"hello">>},{e,null}] =
        jeysn:decode(B1, DOpts),

    [{a,42},{b,false},{c,null},{d,<<"hello">>},{e,false}] =
        jeysn:decode(B2, DOpts),

    [{a,42},{b,false},{c,null},{d,<<"hello">>}] =
        jeysn:decode(B3, DOpts),


    A = {array,
         [R
          , #foo{b=true, c=[1,2,3], d = #bar{boo = <<"boo">>}, e = R}
          , #bar{boo=1, baz=2, bing=3}
         ]},

    RI1 = RI#{bar => record_info(fields, bar)},

    B4 = jeysn:encode(A, [{records, RI1}]),

    [[{a,42},{b,false},{c,null},{d,<<"hello">>},{e,null}],
     [{a,42},{b,true},{c,[1,2,3]},{d,[{boo,<<"boo">>},{baz,null},{bing,null}]},
      {e,[{a,42},{b,false},{c,null},{d,<<"hello">>},{e,null}]}],
     [{boo,1},{baz,2},{bing,3}]] =
        Term = jeysn:decode(B4, DOpts),

    B4 = jeysn:encode(Term),

    ok.
