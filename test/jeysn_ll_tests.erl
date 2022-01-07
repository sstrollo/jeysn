-module(jeysn_ll_tests).

-include_lib("eunit/include/eunit.hrl").

test1_test() ->
    T = jeysn_ll:init_string("\"a\" \"b\" \"c\" \"x1x2x3\" \"atom\" \"bye\""),

    {string,"a"} = jeysn_ll:next_token(T, string),

    {string,<<"b">>} = jeysn_ll:next_token(T, binary),

    {string,c} = jeysn_ll:next_token(T, atom),

    {string,<<"x1x2x3">>} = jeysn_ll:next_token(T, existing_atom),

    {string,atom} = jeysn_ll:next_token(T, existing_atom),

    {string,<<"bye">>} = jeysn_ll:next_token(T),

    eof = jeysn_ll:next_token(T),

    ok.


test1a_test() ->
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



test2a_test() ->
    ['{', {string,<<"foo:bar">>}, ':', 42, '}'] =
        tokens("{ \"foo:bar\" : 42 }"),
    ok.

test2b_test() ->
    ['{', {string,[foo|bar]}, ':', 42, '}'] =
        tokens([{string,{atom,$:}}], "{ \"foo:bar\" : 42 }"),
    ok.

test2c_test() ->
    ['{', {string,"foo:bar"}, ':', 42, '}'] =
        tokens([{string,string}], "{ \"foo:bar\" : 42 }"),
    ok.

%% 0x1D11E =  0001  11001 0001  0001 1110
%% UTF-8: 11110000 10011001 10000100 10011110


test3_test() ->
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



numbers_test() ->
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
