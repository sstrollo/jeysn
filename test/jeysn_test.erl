-module(jeysn_test).
-compile(export_all).

test() ->
    try
        ok = test1(),
        ok = test1a(),
        ok = test2a(),
        ok = test2b(),
        ok = test2c(),
        ok = test3(),
        ok = test_numbers(),
        ok = test_files(),
        ok = test_records(),
        io:format("OK\n", []),
        erlang:halt(0)
    catch
        _Err:_What:_Stack ->
            io:format("~p: ~p\n~p\n", [_Err, _What, _Stack]),
            erlang:halt(1)
    end.

%% ------------------------------------------------------------------------

test1() ->
    T = jeysn_ll:init_string("\"a\" \"b\" \"c\" \"x1x2x3\" \"atom\" \"bye\""),

    {string,"a"} = jeysn_ll:next_token(T, string),

    {string,<<"b">>} = jeysn_ll:next_token(T, binary),

    {string,c} = jeysn_ll:next_token(T, atom),

    {string,<<"x1x2x3">>} = jeysn_ll:next_token(T, existing_atom),

    {string,atom} = jeysn_ll:next_token(T, existing_atom),

    {string,<<"bye">>} = jeysn_ll:next_token(T),

    eof = jeysn_ll:next_token(T),

    ok.


test1a() ->
    T = jeysn_ll:init_string([{string,string}],
                          "\"a\" \"b\" \"c\" \"x1x2x3\" \"atom\" \"bye\""),

    {string,"a"} = jeysn_ll:next_token(T, string),

    {string,<<"b">>} = jeysn_ll:next_token(T, binary),

    {string,c} = jeysn_ll:next_token(T, atom),

    {string,<<"x1x2x3">>} = jeysn_ll:next_token(T, existing_atom),

    {string,atom} = jeysn_ll:next_token(T, existing_atom),

    {string,"bye"} = jeysn_ll:next_token(T),

    eof = jeysn_ll:next_token(T),

    ok.



test2a() ->
    ['{', {string,<<"foo:bar">>}, ':', 42, '}'] =
        tokens("{ \"foo:bar\" : 42 }"),
    ok.

test2b() ->
    ['{', {string,[foo|bar]}, ':', 42, '}'] =
        tokens([{string,{atom,$:}}], "{ \"foo:bar\" : 42 }"),
    ok.

test2c() ->
    ['{', {string,"foo:bar"}, ':', 42, '}'] =
        tokens([{string,string}], "{ \"foo:bar\" : 42 }"),
    ok.

%% 0x1D11E =  0001  11001 0001  0001 1110
%% UTF-8: 11110000 10011001 10000100 10011110


test3() ->
    L =
        [{"\\u0024",          <<16#24>>}                    % CodePoint 0x24
         , {"\\u00a2",        <<16#c2,16#a2>>}              % CodePoint 0xA2
         , {"\\u20aC",        <<16#e2,16#82,16#ac>>}        % CodePoint 0x20AC
         , {"\\uD800\\uDF48", <<16#f0,16#90,16#8d,16#88>>}  % CodePoint 0x10348
         , {"\\uD834\\uDD1E", <<16#1d11e/utf8>>}            % CodePoint 0x1D11E

         , {"\\u0041\\u2262\\u0391\\u002E",
            <<16#41, 16#E2, 16#89, 16#A2, 16#CE, 16#91, 16#2E>>,
            [16#0041, 16#2262, 16#0391, 16#002e]}
         , {"\\uD55C\\uAD6D\\uC5B4",
            <<16#ED, 16#95, 16#9C, 16#EA, 16#B5, 16#AD, 16#EC, 16#96, 16#B4>>,
            [16#D55C, 16#AD6D, 16#C5B4]}
         , {"\\u65E5\\u672C\\u8A9E",
            <<16#E6, 16#97, 16#A5, 16#E6, 16#9C, 16#AC, 16#E8, 16#AA, 16#9E>>}
        ],
    lists:foreach(
      fun ({Esc, Bin}) ->
              case tokens([$",Esc,$"]) of
                  [{string,Bin}] ->
                      %% io:format("~tp\n", [Bin]),
                      ok;
                  Other ->
                      io:format("~p != ~p\n", [Other, Bin]),
                      exit(error)
              end;
          ({Esc, Bin, UnicodeList}) ->
              case tokens([$",Esc,$"]) of
                  [{string,Bin}] ->
                      %% Note: also see unicode:characters_to_binary/1
                      case unicode:characters_to_list(Bin, utf8) of
                          UnicodeList ->
                              %% io:format("~tp\n", [Bin]),
                              ok;
                          Other ->
                              io:format("~p != ~p\n", [Other, Bin]),
                              exit(error)
                      end;
                  Other ->
                      io:format("~p != ~p\n", [Other, Bin]),
                      exit(error)
              end
      end, L),
    ok.


tokens(Buffer) ->
    tokens([], Buffer).

tokens(Opts, Buffer) ->
    S = jeysn_ll:init_string(Opts, Buffer),
    tokens(S, [], jeysn_ll:next_token(S)).

tokens(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokens(_S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), Err};
tokens(S, Tokens, Token) ->
    tokens(S, [Token|Tokens], jeysn_ll:next_token(S)).



test_numbers() ->
    test_integer(max32()),
    test_integer(max31()),
    test_integer(max64()),
    test_integer(max63()),
    test_number_number(max64()),
    test_number_number(0 - (max63()-1)),
    expect_token("1.0e2", 100.0),
    expect_token("1.0e-2", 0.01),
    expect_token("-1.0e2", -100.0),
    expect_token("-1.0e-2", -0.01),
    expect_token("1e2", 100.0),
    expect_token("1e-2", 0.01),
    expect_token("-1e2", -100.0),
    expect_token("-1e-2", -0.01),
    ok.

test_integer(N) ->
    test_number(N - 1),
    test_number(N),
    test_number(N+1),
    test_number(N+2345),
    test_number(N*10),
    test_number(N div 10),
    test_number(0 - (N - 1)),
    test_number(0 - (N)),
    test_number(0 - (N+1)),
    test_number(0 - (N+2345)),
    test_number(0 - (N*10)),
    test_number(0 - (N div 10)),
    ok.

max31() -> 2147483647.
max32() -> 4294967295.
max64() -> 18446744073709551615.
max63() -> 9223372036854775807.

test_number_number(N) ->
    N = jeysn_ll:next_token(jeysn_ll:init_string(integer_to_list(N))),
    ok.

test_number(N) when is_integer(N) ->
    %% io:format("~p\n", [N]),
    State = jeysn_ll:init_string(integer_to_list(N)),
    case jeysn_ll:next_token(State) of
        N ->
            ok;
        {number, Bin} ->
            N = binary_to_integer(Bin)
    end,
    eof = jeysn_ll:next_token(State),
    ok.

expect_token(String, Expect) ->
    %%io:format("string: ~999p expecting: ~999p ... ", [String, Expect]),
    State = jeysn_ll:init_string(String),
    Expect = jeysn_ll:next_token(State),
    eof = jeysn_ll:next_token(State),
    %%io:format("ok\n", []),
    ok.

test_files() ->
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

test_records() ->
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

