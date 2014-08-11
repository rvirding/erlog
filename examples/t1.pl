%%% -*- mode: prolog -*-

a(1).
a(2).
a(3).

b(1).
b(2).
b(3).

c(1).
c(2).
c(3).

d(4).
d(5).
d(6).

e(4).
e(5).
e(6).

f(4).
f(5).
f(6).

d(X,d(X)).

e(X, Y) :- a(X), !, b(Y).
e(a, b).

%%f(X) :- X = 'abc\x111\def'.
%%f(X) :- :- a(X).

perm([], []).
perm([X|Xs], Ys1) :- perm(Xs, Ys), insert(Ys, X, Ys1).

nrev([], []).
nrev([H|T], L1) :- nrev(T, L), append(L, [H], L1).

testnrev(0, _).
testnrev(N, L) :-
    nrev(L, _),
    !,
    N1 is N - 1,
    testnrev(N1, L).

for(0, L) :- fail.
for(N, L) :- nrev(L, _), fail.
for(N, L) :- N1 is N-1, for(N1, L).

x(X) --> [x], {X = 1}.
x(X) --> [y,z], {X = 2}.

%% For testing cuts.

ct1(X, Y) :- a(X), b(Y), Y \= X.
ct1(X, Y) :- a(X), !, b(Y), Y \= X.
ct1(X, Y) :- a(X), b(Y).

ct2(X, Y, Z) :- a(X), b(Y), c(Z), Z \= X, Z \= Y.
ct2(X, Y, Z) :- a(X), !, b(Y), !, c(Z), Z \= X, Z \= Y.
ct2(X, Y, Z) :- a(X), b(Y), c(Z), Z \= X, Z \= Y.

ct3(X, Y, Z) :- ( a(X) -> b(Y), Y \= X ), c(Z), Z \= X, Z \= Y.
ct3(X, Y, Z) :- ( a(X) -> b(Y), Y \= X ), !, c(Z), Z \= X, Z \= Y.
ct3(X, Y, Z) :- a(X), b(Y), Y \= X, c(Z), Z \= X, Z \= Y.

ct4(X, Y, Z) :- a(X), ( b(Y), display(Y), Y \= X, c(Z), Z \= X, Z \= Y -> true ).
ct4(X, Y, Z) :- a(X), ( b(Y), display(Y), !, Y \= X, c(Z), Z \= X, Z \= Y -> true ).
ct4(X, Y, Z) :- a(X), ( b(Y), Y \= X, c(Z), Z \= X, Z \= Y -> true ).

ac(1).
ac(2) :- !.
ac(3).

bc(1).
bc(2) :- !.
bc(3).

cc(1).
cc(2) :- !.
cc(3).

dc1(X, Y, Z) :- ac(X), bc(Y), cc(Z).
dc1(X, Y, Z) :- ac(X), bc(Y), !, cc(Z).
dc1(X, Y, Z) :- ac(X), bc(Y), cc(Z).

%% End of file
