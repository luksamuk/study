%% :- use_module(library(simplex)).

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
% ?- knapsack_integral(S), variable_value(S, x(1), X1), variable_value(S, x(2), X2).



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
%% ?- moedas(S),
%%    variable_value(S, m(1), M1),
%%    variable_value(S, m(5), M5),
%%    variable_value(S, m(20), M20).



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
% X1 = 0, X2 = 5.

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
% X1 = 2, X2 = 0, X3 = 1.

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
% X1 = 0, X2 = 5.
