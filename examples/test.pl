/* -*-Prolog -*-*/
:- dynamic(a/0).
%:- dynamic b/0.
%:- dynamic c/0.
%:- dynamic d/0.

a :- write(a(1)), nl.
a :- write(a(2)), nl.
a :- write(a(3)), nl.

b :- write(b(1)), nl.
b :- write(b(2)), nl.
b :- write(b(3)), nl.

c :- write(c(1)), nl.
c :- write(c(2)), nl.
c :- write(c(3)), nl.

d :- write(d(1)), nl.

t1 :- a, !, b , c.
t1 :- d.

t2 :- (a, b; c).
t2 :- d.

t3 :- (a, !, b; c).
t3 :- d.

t4 :- (a ; b, c).
t4 :- d.

t5 :- (a ; b, !, c).
t5 :- d.
