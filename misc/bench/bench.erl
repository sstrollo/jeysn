-module(bench).

-compile(export_all).


files() ->
    lists:filter(
      fun ("") -> false; (_) -> true end,
      string:split(
        case os:getenv("JSON") of
            false -> "";
            _Res -> _Res
        end, " ", all)).

jeysn() ->
    Modules = [jeysn],
    r(standard_io, Modules, files()).

%% Decode with objects represented maps
o() ->
    other(
      [jeysn
       , {eyaml, fun (Bin) -> eyaml:parse(Bin) end}
       , {jsx, fun (Bin) -> jsx:decode(Bin, [{return_maps,true}]) end}
       , jsone
      ]).

%% Decode with objects represented as lists
l() ->
    other(
      [{jeysn, fun (Bin) -> jeysn:decode(Bin, [{object,list}]) end}
       , {eyaml, fun (Bin) -> eyaml:parse(Bin, #{mapping_as_list => true}) end}
       , {jsx, fun (Bin) -> jsx:decode(Bin, [{return_maps,false}]) end}
       , {jsone,
          fun (Bin) -> jsone:decode(Bin, [{object_format, proplist}]) end}
      ]).

other(Modules) ->
    r(standard_io, Modules, files()).

d() ->
    Modules = [dloop1, dloop2, dloop3],
    r(standard_io, Modules, files()).

r(Out, Modules, Files) ->
    _ = application:start(jeysn),
    fmt_header(Out),
    lists:foreach(
      fun (File) ->
              lists:foreach(
                fun (Mod0) ->
                        {Mod, Fun} =
                            case Mod0 of
                                Module when is_atom(Module) ->
                                    {Module,
                                     fun (Bin) -> Module:decode(Bin) end};
                                {Module, Function} ->
                                    {Module, Function}
                            end,
                        {ok, B} = file:read_file(File),
                        case run_decode(Fun, B) of
                            {ok, {AvgTime, _, Diff}} ->
                                fmt(Out, File, Mod, AvgTime/1000, Diff);
%                                io:format("~s  ~w: ~pms\n~s\n",
%                                          [File, Mod, AvgTime/1000,
%                                           fmt_info(Diff)]);
                            {error, _Err} ->
                                io:format("~s:  ERROR: ~0p\n", [File, _Err])
                        end
                end, Modules),
              fmt_empty(Out)
      end, Files),
    fmt_footer(Out),
    ok.

fmt(Out, File, Mod, Time, Info) ->
    %% |Filename|Module|Time|Reds|Mem|Heap|GC|
    io:format(Out,
              "| ~20s | ~9s | ~9.3f | ~10w | ~9w | ~9w | ~7w |\n",
              [File, Mod, Time | ii(Info)]).

fmt_header(Out) ->
    fmt_footer(Out),
    io:format(Out,
              "| ~20s | ~9s | ~9s | ~10s | ~9s | ~9s | ~7s |\n"
              "| ~20s | ~9s | ~9s | ~10s | ~9s | ~9s | ~7s |\n",
              ["Filename", "Module", "Time", "Reductions",
               "Memory", "Heap", "Min GCs",
               "", "", "(ms)", "", "(bytes)", "(bytes)", ""
              ]),
    io:format(Out,
              "| -------------------- | --------- | --------- | ---------- "
              "| --------- | --------- | ------- |\n", []),
    ok.

fmt_footer(Out) ->
    io:format(Out,
              "+-----------------------------------------------------------"
              "----------------------------------+\n", []),
    ok.

fmt_empty(Out) ->
    io:format(Out,
              "|                      |           |           "
              "|            |           |           |         |\n",
              []),
    ok.


run_decode(Fun, Bin) ->
    Parent = self(),
    {Pid, Ref} =
        spawn_opt(
          fun () ->
                  erlang:group_leader(self(), self()),
                  erlang:garbage_collect(),
                  Before = info(),
                  Time1 = element(1, timer:tc(Fun, [Bin])),
                  Time2 = element(1, timer:tc(Fun, [Bin])),
                  Time3 = element(1, timer:tc(Fun, [Bin])),
                  Time4 = element(1, timer:tc(Fun, [Bin])),
                  Time5 = element(1, timer:tc(Fun, [Bin])),
                  AvgTime = (Time1 + Time2 + Time3 + Time4 + Time5) div 5,
                  After = info(),
                  Diff = info_diff(Before, After),
                  Parent ! {result, self(),
                            {AvgTime,
                             [Time1, Time2, Time3, Time4, Time5],
                             Diff}},
                  ok
          end, [monitor]),
    receive
        {result, Pid, Res} ->
            {ok, Res};
        {'DOWN', Ref, process, Pid, _Info} ->
            {error, _Info}
    end.

-record(info, {reductions, memory, stack, heap, minor_gcs, gci}).

info() ->
    [{reductions, Reds},
     {memory, Bytes},
     {stack_size, StackSize},
     {heap_size, HeapSize},
     {garbage_collection, GC},
     {garbage_collection_info, GCI}] =
        process_info(self(),
                     [reductions, memory, stack_size, heap_size,
                      garbage_collection, garbage_collection_info]),
    #info{reductions = Reds,
          memory = Bytes,
          stack = StackSize,
          heap = HeapSize,
          minor_gcs = proplists:get_value(minor_gcs, GC, 0),
          gci = GCI}.

info_diff(Old, New) ->
    #info{reductions = New#info.reductions - Old#info.reductions,
          memory     = New#info.memory     - Old#info.memory,
          stack      = New#info.stack      - Old#info.stack,
          heap       = New#info.heap       - Old#info.heap,
          minor_gcs  = New#info.minor_gcs  - Old#info.minor_gcs,
          gci = [{Key, Value - proplists:get_value(Key, Old#info.gci, 0)} ||
                    {Key, Value} <- New#info.gci]}.

fmt_info(Info) ->
%    io_lib:format("reds: ~w  memory: ~9.9.0wbytes  heap: ~9.9.0wbytes  minor gcs: ~w",


    io_lib:format("  reductions memory (bytes) heap (bytes)  minor gcs\n"
                  "  ~10w ~14w ~14w ~10w"
                  , ii(Info)).

ii(Info) ->
    [Info#info.reductions,
     Info#info.memory,
     Info#info.heap * erlang:system_info(wordsize),
     Info#info.minor_gcs].
