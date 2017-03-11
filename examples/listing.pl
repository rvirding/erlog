%% -*- mode: prolog -*-
%% Copyright (c) 2008-2017 Robert Virding
%%
%% A very simple listing library.

listing(P/Ar) :- !, listing(P, Ar).
listing(P) :- listing(P, 0).

listing(P, Ar) :-
    length(As, Ar),
    H =.. [P|As],
    clause(H, B),
    numbervars((H,B), 0, _),
    '$print_clause'(H, B),
    fail.

'$print_clause'(H, true) :-
    !, writeq(H), put_char('.'), nl.
'$print_clause'(H, B) :-
    writeq(H), put_char(' '), write((:-)), nl,
    '$print_body'(B).

'$print_body'((G, Gs)) :-
    !,
    '$print_goal'(G),
    '$print_body'(Gs).
'$print_body'(G) :-
    '$print_goal'(G).

'$print_goal'(G) :-
    write('        '), writeq(G), put_char('.'), nl.
