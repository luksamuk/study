:- use_module(library(clpfd)).

moves(Js0) --> { member(jug(_, _, 2), Js0) }.
moves(Js0) --> [from_to(F, T)],
	       { select(jug(F, FC, FF0), Js0, Js1),
		 select(jug(T, TC, TF0), Js1, Js),
		 M #= min(FF0, TC-TF0),
		 FF #= FF0 - M,
		 TF #= TF0 + M },
	       moves([jug(F, FC, FF), jug(T, TC, TF) | Js]).

print_answer([L, Ms]) :-
    format('L = ~a, Ms = ', [L]),
    writeln(Ms).

find_solutions(Ms, L) :-
   length(Ms, L), 
   phrase(moves([jug(a,4,0), jug(b,3,0), jug(c,7,7)]), Ms).

?- findnsols(10, [L, Ms], find_solutions(Ms, L), Ans),
   maplist(print_answer, Ans).
