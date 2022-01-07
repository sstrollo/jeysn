-module(dloop2).

-define(vinit, [fun (_, _) -> ok end]).

-define(cn(__N, __S), begin (hd(__N))(__S, tl(__N)) end).
-define(n(__F, __N), [fun __F/2 | __N]).

-include("_dloop.hrl").
