%%% -*- mode: prolog -*-

mt([], [], []).
mt([X|Xs], [Y|Ys], [Z|Zs]) :-
	m(X, Y, Z),
	mt(Xs, Ys, Zs).

m(0, _, 0).
m(1, X, Y) :- m1(X, Y).
m(2, X, Y) :- m2(X, Y).

m1(0, 0).
m1(1, 0).
m1(2, 1).

m2(0, 0).
m2(1, 2).
m2(2, 2).

g1([0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2]).
g2([0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2]).
g3([0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2]).

solution(Sol) :-
	g1(G1),
	g2(G2),
	g3(G3),
	solution([], [G1,G2,G3], Sol).

solution(State, New, Sol) :-
	once((union(New, State, State1),	%Add new elements to state
	      subtract(New, State, New1))),	%subtract old elements
	solution(New1, State, State1, Sol).

solution([], State, _, State).
solution([New|News], Old, State, Sol) :-
	once(products(Old, [New|News], Prods)),
	solution(State, Prods, Sol).

products(Old, New, Prods) :-
	products(Old, New, Prods0, Prods1),
	products(New, Old, Prods1, Prods2),
	products(New, New, Prods2, []),
	sort(Prods0, Prods).

products([], _, P, P).
products([X|Xs], Ys, P0, P) :-
	once(products1(Ys, X, P0, P1)),
	products(Xs, Ys, P1, P).

products1([], _, P, P).
products1([Y|Ys], X, [Z|P0], P) :-
	once(mt(X, Y, Z)),
	products1(Ys, X, P0, P).

union([], U, U).
union([X|Xs], Ys, U) :-
	(   member(X, Ys) ->
	    union(Xs, Ys, U)
	;   U = [X|U1],
	    union(Xs, Ys, U1)
	).

subtract([], _, []).
subtract([X|Xs], Ys, Sub) :-
	(   member(X, Ys) ->
	    subtract(Xs, Ys, Sub)
	;   Sub = [X|Sub1],
	    subtract(Xs, Ys, Sub1)
	).
