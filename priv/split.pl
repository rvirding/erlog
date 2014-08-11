%-*- Prolog -*-


split(Head, Tail, HeadLength, FullList) :-
	length(Head, HeadLength),
	append(Head, Tail, FullList).
