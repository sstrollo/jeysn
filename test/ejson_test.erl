-module(ejson_test).
-compile(export_all).

test() ->
    try
        ok = test1(),
        ok = test1a(),
        ok = test2a(),
        ok = test2b(),
        ok = test2c(),
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
