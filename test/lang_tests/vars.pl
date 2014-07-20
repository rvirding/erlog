%-*-Prolog-*-


test(File) :-
	nonvar(3),
	nonvar(x),
	nonvar([]),
	nonvar(File),
	\+var(File),
	var(X),
	\+nonvar(X).
	

	