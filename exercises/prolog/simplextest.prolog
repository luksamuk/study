:- use_module(library(simplex)).

%% http://eu.swi-prolog.org/pldoc/man?section=simplex

radiation(S) :-
    gen_state(S0),
    post_constraints(S0, S1),
    minimize([0.4*x1, 0.5*x2], S1, S).

post_constraints -->
    constraint([0.3*x1, 0.1*x2] =< 2.7),
    constraint([0.5*x1, 0.5*x2] = 6),
    constraint([0.6*x1, 0.4*x2] >= 6),
    constraint([x1] >= 0),
    constraint([x2] >= 0).

% ?- radiation(S), variable_value(S, x1, Val1), variable_value(S, x2, Val2).
%@ S = solved(tableau(row(z, [0, 0, 0, 0, 0, 0|...], 21r4), [0, 1, 2, 3, 4, 5, 6|...], [1, 0, 1, 0, 1, 0, 0|...], [row(x2, [0, 0, 0, 0, 0|...], 9r2), row(x1, [0, 0, 0, 0|...], 15r2), row(0, [1, -1, 0|...], 9r2), row(4, [0, 0|...], 3r10), row(2, [0|...], 15r2)]), [0-7, 0-6, 0-5, 0-3, 0-1], []),
%@ Val1 = 15r2,
%@ Val2 = 9r2.

knapsack(S) :-
    knapsack_constraints(S0),
    maximize([7*x(1), 4*x(2)], S0, S).

knapsack_constraints(S) :-
    gen_state(S0),
    constraint([6*x(1), 4*x(2)] =< 8, S0, S1),
    constraint([x(1)] =< 1, S1, S2),
    constraint([x(2)] =< 2, S2, S).

knapsack_integral(S) :-
    knapsack_constraints(S0),
    constraint(integral(x(1)), S0, S1),
    constraint(integral(x(2)), S1, S2),
    maximize([7*x(1), 4*x(2)], S2, S).

% ?- knapsack(S), variable_value(S, x(1), X1), variable_value(S, x(2), X2).
%@ S = solved(tableau(row(z, [0, 1, 1, 0, 0], 9), [0, 1, 2, x(1), x(2)], [1, 1, 1, 1, 1], [row(0, [1, 3r2, -1r4, 0, 0], 3r2), row(x(1), [0, 1, 0, 1|...], 1), row(x(2), [0, -3r2, 1r4|...], 1r2)]), [0-2, 0-1, 0-0], []),
%@ X1 = 1,
%@ X2 = 1r2.
% ?- knapsack_integral(S), variable_value(S, x(1), X1), variable_value(S, x(2), X2).
%@ S = solved(tableau(row(z, [7, 0, 0, 4, 0, 0|...], 8), [0, 1, 2, 3, 4, x(1), x(...)], [1, 1, 0, 1, 1, 1, 1], [row(x(1), [1, 0, 0, 0, 0|...], 0), row(x(2), [0, 0, 0, 1|...], 2), row(1, [0, 1, -1|...], 1), row(4, [-6, 0|...], 0)]), [0-4, 0-3, 0-2, 0-0], [x(1), x(2)]),
%@ X1 = 0,
%@ X2 = 2.



% Dado que tenhamos:
%
%     - 3 moedas de 1 centavo
%     - 20 moedas de 5 centavos
%     - 10 moedas de 20 centavos
%
% Encontrar o número mínimo de cada uma das moedas de forma a totalizar 111 centavos.

moedas(S) :-
    gen_state(S0),
    moedas(S0, S).

moedas -->
    constraint([m(1), 5*m(5), 20*m(20)] = 111),
    constraint([m(1)] =< 3),
    constraint([m(5)] =< 20),
    constraint([m(20)] =< 10),
    constraint([m(1)] >= 0),
    constraint([m(5)] >= 0),
    constraint([m(20)] >= 0),
    constraint(integral(m(1))),
    constraint(integral(m(5))),
    constraint(integral(m(20))),
    minimize([m(1), m(5), m(20)]).

% Para resolver, faça a query:
%% ?- moedas(S), variable_value(S, m(1), M1), variable_value(S, m(5), M5), variable_value(S, m(20), M20).
%@ S = solved(tableau(row(z, [-4, -19, 0, 0, 0, 0|...], 8), [0, 1, 2, 3, 4, 5, 6|...], [1, 1, 1, 0, 1, 0, 1|...], [row(4, [1, 0, 0, 0, 1|...], 2), row(2, [0, 1, 1, -1|...], 5), row(m(20), [0, 1, 0|...], 5), row(m(5), [1, 0|...], 2), row(m(1), [-5|...], 1), row(8, [...|...], 2), row(..., ..., ...)]), [0-9, 0-8, 0-7, 0-5, 0-3, 0-1, 0-0], [m(20), m(5), m(1)]),
%@ M1 = 1,
%@ M5 = 2,
%@ M20 = 5.



%% Exercícios de http://www.decom.ufop.br/gustavo/bcc342/Aula-5_Simplex.pdf

ex01(S) :-
    gen_state(S0),
    ex01(S0, S).

ex01 -->
    constraint([3*x1, 2*x2] =< 18),
    constraint([x1, x2] =< 5),
    constraint([x1] =< 4),
    constraint([x1] >= 0),
    constraint([x2] >= 0),
    maximize([4*x1, 8*x2]).

% ?- ex01(S), variable_value(S, x1, X1), variable_value(S, x2, X2).
%@ S = solved(tableau(row(z, [0, 0, 4, -4, 0, 8|...], 40), [0, 1, 2, 3, 4, 5, 6|...], [1, 0, 1, 0, 1, 1, 1|...], [row(x2, [0, 0, 1, -1, 0|...], 5), row(x1, [0, 0, -1, 1|...], 0), row(4, [0, 0, 1|...], 4), row(0, [1, -1|...], 5), row(6, [0|...], 8)]), [0-6, 0-5, 0-4, 0-3, 0-1], []),
%@ X1 = 0,
%@ X2 = 5.

ex02(S) :-
    gen_state(S0),
    ex02(S0, S).

ex02 -->
    constraint([2*x1, 3*x2, x3] =< 5),
    constraint([4*x1, 2*x2, 2*x3] =< 11),
    constraint([3*x1, 2*x2, 2*x3] =< 8),
    constraint([x1] >= 0),
    constraint([x2] >= 0),
    constraint([x3] >= 0),
    maximize([5*x1, 4*x2, 3*x3]).

% ?- ex02(S), variable_value(S, x1, X1), variable_value(S, x2, X2), variable_value(S, x3, X3).
%@ S = solved(tableau(row(z, [0, 0, 1, -1, 0, 0|...], 13), [0, 1, 2, 3, 4, 5, 6|...], [1, 0, 1, 0, 1, 0, 1|...], [row(x3, [0, 0, -5, 5, 0|...], 1), row(x2, [0, 0, -1, 1|...], 0), row(x1, [0, 0, 4|...], 2), row(0, [1, -1|...], 1), row(7, [0|...], 1), row(4, [...|...], 2)]), [0-8, 0-7, 0-6, 0-5, 0-3, 0-1], []),
%@ X1 = 2,
%@ X2 = 0,
%@ X3 = 1.

ex03(S) :-
    gen_state(S0),
    ex03(S0, S).

ex03 -->
    constraint([3*x1, 2*x2] =< 18),
    constraint([x1, x2] =< 5),
    constraint([x1] =< 4),
    constraint([x1] >= 0),
    constraint([x2] >= 0),
    maximize([4*x1, 8*x2]).

% ?- ex03(S), variable_value(S, x1, X1), variable_value(S, x2, X2).
%@ S = solved(tableau(row(z, [0, 0, 4, -4, 0, 8|...], 40), [0, 1, 2, 3, 4, 5, 6|...], [1, 0, 1, 0, 1, 1, 1|...], [row(x2, [0, 0, 1, -1, 0|...], 5), row(x1, [0, 0, -1, 1|...], 0), row(4, [0, 0, 1|...], 4), row(0, [1, -1|...], 5), row(6, [0|...], 8)]), [0-6, 0-5, 0-4, 0-3, 0-1], []),
%@ X1 = 0,
%@ X2 = 5.
