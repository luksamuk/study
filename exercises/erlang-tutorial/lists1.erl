-module(lists1).
-export([min/1, max/1, min_max/1]).

min(L) ->
    lists:nth(1, L).

max(L) ->
    lists:nth(length(L), L).

min_max(L) ->
    {min(L), max(L)}.
