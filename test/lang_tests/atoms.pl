%-*-Prolog-*-


test(File) :-
	len(X,Len),
	atom_chars(abc, X),
	atom_length(abc, Len).


len([], 0).
len([H|T], Length) :-
	len(T, L1), Length is L1 + 1.
