% Parcialmente copiado de https://t3x.org/bits/prolog-6-slides.pdf

% Dados do grafo do enunciado
edge(a, b).
edge(b, a).
edge(a, d).
edge(d, a).
edge(b, d).
edge(d, b).
edge(a, c).
edge(c, a).

path(A, B, [A, B]) :- edge(A, B).
path(A, B, [A | CB]) :-
	edge(A, C),
	path(C, B, CB).

visited([], []).
visited([A, B | Rest], V) :-
	append(R, V1, V),
	path(A, B, R),
	visited(Rest, V1).

hamiltonian_path(Path) :-
	once(visited(Path, V)),
	setof(X, member(X, V), Set),
	length(V, N),
	length(Set, N).

