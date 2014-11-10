% ����
% ���������: run(S).
% ������ ������� ����� �� ������ ���������� ������ f.
% ���������: run1(�).
% ������ ������� ���������� ������ f.

f("p1", 100).
f("p2", 200).
f("p3", 300).

run(_):-
	retract(result(_)),
	false.
run(_):-
	assert(result(0)),
	false.
run(_):-
	f(_, Amnt),
	retract(result(S)),
	Snew is S + Amnt,
	assert(result(Snew)),
	false.
run(S):-
	result(S).

run1(C):-
	calc_count([], 0, C).
calc_count(L, C, Cc):-
	f(Key, _Amnt),
	\+(append(_, [Key|_], L)),
	Cn is C + 1,
	calc_count([Key|L], Cn, Cc).
calc_count(_, C, C).

test_all:-
	run(600),
	run1(3).

% :- test_all.
