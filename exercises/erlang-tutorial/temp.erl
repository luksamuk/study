-module(temp).
-export([f2c/1, c2f/1]).

f2c(F) ->
    5 * (F - 32) / 9.

c2f(C) ->
    (9 * C / 5) + 32.
