test_p(R):-
use(erlog_parallel),
spawn(writeln("aaa"), P1),
spawn(writeln("bbb"), P2),
spawn(parrallel(5, check([1,Z,fii(R)])), P3),
B = 4,
C = 2,
A1 = a,
A2 = b,
A3 = c,
get(D),
join([P1, P2, P3], 5000),
check(P3, _).

get(1).
get(2).

parrallel(F1, F2):-
  BP = 5,
  CP = 7,
  foo(F1, F2).

foo(1, a).
foo(2, b).
foo(3, c).
foo(4, d).
foo(5, e).
foo(5, check([1, 2, fii(e)])).

fii(e).
