%%
%% Experimental and debugging stuff
%%
-module(wip).

-compile(export_all).

%% ------------------------------------------------------------------------

file(FName) ->
    BufSz = 16,
    case file:open(FName, [read,binary,raw]) of
        {ok, Fd} ->
            F = fun () -> file:read(Fd, BufSz) end,
            floop(jeysn_ll:init(), F, []);
        _Err ->
            io:format("~p\n", [_Err]),
            _Err
    end.

file_data(S, {ok, Buf}) ->
    jeysn_ll:data(S, Buf);
file_data(S, eof) ->
    jeysn_ll:data(S, eof).

floop(State, ReadF, Res) ->
    case jeysn_ll:next_token(State, string) of
        more ->
            _Debug = jeysn_ll:debug(State),
%            io:format("Debug: ~p\n", [_Debug]),
            file_data(State, ReadF()),
            floop(State, ReadF, Res);
        eof ->
            _Debug = jeysn_ll:debug(State),
%            io:format("Debug: ~p\n", [_Debug]),
            lists:reverse(Res);
        {error, Error} ->
            {error, Error, jeysn_ll:get_position(State)};
        Token ->
            io:format("Token: ~p\n", [Token]),
            floop(State, ReadF, [Token|Res])
    end.

%% ------------------------------------------------------------------------

t() ->
    tokenize(<<"{\"foo:bar\": 42}">>).

tokenize(Buffer) ->
    S = jeysn_ll:init_string(Buffer),
    tokenize(S, [], jeysn_ll:next_token(S, {existing_atom,$:})).

tokenize(_S, Tokens, eof) ->
    lists:reverse(Tokens);
tokenize(S, Tokens, {error, Err}) ->
    {error, lists:reverse(Tokens), jeysn_ll:debug(S), Err};
tokenize(S, Tokens, Token) ->
    tokenize(S, [Token|Tokens], jeysn_ll:next_token(S, {existing_atom,$:})).

%% ------------------------------------------------------------------------
