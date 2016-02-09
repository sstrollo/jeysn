-module(eparse).

-export([file/1]).
-on_load(on_load/0).


file(FName) ->
    BufSz = 16,
    case file:open(FName, [read,binary,raw]) of
        {ok, Fd} ->
            F = fun () -> file:read(Fd, BufSz) end,
            ploop(init(42), F, [], F());
        _Err ->
            io:format("~p\n", [_Err]),
            _Err
    end.

ploop(_State, _MoreBytesF, Res, eof) ->
    Res;
ploop(State, MoreBytesF, Res, {ok, Buf}) ->
    case parse(State, Buf) of
        {1} ->
            io:format("Debug: ~p\n", [debug(State)]),
            ploop(State, MoreBytesF, Res, MoreBytesF());
        {2, Symbol} ->
            io:format("Symbol: ~p\n", [Symbol]),
            ploop(State, MoreBytesF, [Symbol|Res], {ok, []});
        {3, Error} ->
            {error, Error}
    end.

%% -> more_bytes | symbol | error
parse(_State, _Buf) ->
    ok.

init(_X) ->
    ok.

debug(_x) ->
    ok.


on_load() ->
    erlang:load_nif("eparse_nif", 0).

