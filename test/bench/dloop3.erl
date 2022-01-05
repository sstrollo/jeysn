-module(dloop3).

-define(vinit, fun (_) -> ok end).

-define(cn(__N, __S), begin (__N)(__S) end).
-define(n(__F, __N), fun (__S) -> (fun __F/2)(__S, __N) end).

-include("_dloop.hrl").
