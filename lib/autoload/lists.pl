perm([], []).
perm([X|Xs], Ys1) :- perm(Xs, Ys), insert(Ys, X, Ys1).
