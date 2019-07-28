-module(test).
-export([factorial/1, fib/1, average/1,
         double/1, member/2]).

% Factorial of a number
factorial(0) ->
    1;
factorial(X) when X > 0 ->
    X * factorial(X - 1).


% nth number in fibonacci sequence
fib(0) -> 0;
fib(1) -> 1;
fib(X) when X > 1 ->
    fib(X - 2) + fib(X - 1).


% Average of a list
average(X) when is_list(X) ->
    sum(X) / len(X).

% Sum of a list
sum([H|T]) ->
    H + sum(T);
sum([]) -> 0.

% Length of a list
len([_|T]) ->
    1 + len(T);
len([]) -> 0.


% Transform a list by doubling each element
double([H|T]) ->
    [2 * H | double(T)];
double([]) -> [].


% Whether list contains element
member(H, [H|_]) -> true;
member(H,[_|T]) -> member(H, T);
member(_, []) -> false.

