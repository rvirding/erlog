/* -*-Prolog -*-*/
%%% File    : family.pl
%%% Purpose : Family tree example from Bratko
%%%
%%% This is the family tree example in ch 1 of Bratko.

parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

female(pam).
male(tom).
male(bob).
female(liz).
female(ann).
female(pat).
male(jim).

offspring(X, Y) :- parent(Y, X).

mother(X, Y) :-
	parent(X, Y),
	female(X).

father(X, Y) :-
	parent(X, Y),
	male(X).

grandparent(X, Y) :-
	parent(X, Z),
	parent(Z, Y).

sister(X, Y):-
	parent(Z, X),
	parent(Z, Y),
	female(X),
	X \= Y.

brother(X, Y) :-
	parent(Z, X),
	parent(Z, Y),
	male(X),
	X \= Y.

predecessor(X, Y) :- parent(X, Y).
predecessor(X, Y) :-
	parent(X, Z),
	predecessor(Z, Y).
