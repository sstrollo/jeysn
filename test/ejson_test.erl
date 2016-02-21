-module(ejson_test).
-compile(export_all).

test() ->
    try
        ok = test1(),
        ok = test1a(),
        ok = test2a(),
        ok = test2b(),
        ok = test2c(),
        ok = test3(),
        erlang:halt(0)
    catch
        _Err:_What ->
            io:format("~p: ~p\n~p\n", [_Err, _What, erlang:get_stacktrace()]),
            erlang:halt(1)
    end.

%% ------------------------------------------------------------------------

test1() ->
    T = ejson:init_string("\"a\" \"b\" \"c\" \"x1x2x3\" \"atom\" \"bye\""),

    {token,{string,"a"}} = ejson:next_token(T, string),

    {token,{string,<<"b">>}} = ejson:next_token(T, binary),

    {token,{string,c}} = ejson:next_token(T, atom),

    {token,{string,<<"x1x2x3">>}} = ejson:next_token(T, existing_atom),

    {token,{string,atom}} = ejson:next_token(T, existing_atom),

    {token,{string,<<"bye">>}} = ejson:next_token(T),

    eof = ejson:next_token(T),

    ok.


test1a() ->
    T = ejson:init_string([{string,string}],
                          "\"a\" \"b\" \"c\" \"x1x2x3\" \"atom\" \"bye\""),

    {token,{string,"a"}} = ejson:next_token(T, string),

    {token,{string,<<"b">>}} = ejson:next_token(T, binary),

    {token,{string,c}} = ejson:next_token(T, atom),

    {token,{string,<<"x1x2x3">>}} = ejson:next_token(T, existing_atom),

    {token,{string,atom}} = ejson:next_token(T, existing_atom),

    {token,{string,"bye"}} = ejson:next_token(T),

    eof = ejson:next_token(T),

    ok.



test2a() ->
    [{token,'{'},
     {token,{string,<<"foo:bar">>}},
     {token,':'},
     {token,42},
     {token,'}'}] = tokens("{ \"foo:bar\" : 42 }"),
    ok.

test2b() ->
    [{token,'{'},
     {token,{string,[foo|bar]}},
     {token,':'},
     {token,42},
     {token,'}'}] = tokens([{string,{atom,$:}}], "{ \"foo:bar\" : 42 }"),
    ok.

test2c() ->
    [{token,'{'},
     {token,{string,"foo:bar"}},
     {token,':'},
     {token,42},
     {token,'}'}] = tokens([{string,string}], "{ \"foo:bar\" : 42 }"),
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
        ],
    lists:foreach(
      fun ({Esc, Bin}) ->
              case tokens([$",Esc,$"]) of
                  [{token,{string,Bin}}] ->
                      ok;
                  Other ->
                      io:format("~p != ~p\n", [Other, Bin]),
                      exit(error)
              end
      end, L),
    ok.


tokens(Buffer) ->
    tokens([], Buffer).

tokens(Opts, Buffer) ->
    S = ejson:init_string(Opts, Buffer),
    tokens(S, [], ejson:next_token(S)).

tokens(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokens(_S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), Err};
tokens(S, Tokens, Token) ->
    tokens(S, [Token|Tokens], ejson:next_token(S)).
