% -*-prolog-*-



test(_) :-
	Var = book(one,two,three),
	Two = two,
	arg(2,Var,Two).
