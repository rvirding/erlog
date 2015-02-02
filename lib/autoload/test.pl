test_p(R):-
use(erlog_parallel),
spawn(writeln("aaa"), P1),
spawn(writeln("bbb"), P2),
spawn(foo(5, A), P3),
join([P1, P2, P3], 5000),
check(P3, _),
R = A.


foo(1, a).
foo(2, b).
foo(3, c).
foo(4, d).
foo(5, e).
