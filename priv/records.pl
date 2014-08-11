%-*- Prolog -*-


record(_,[]):- !.
record(RecordName,Fields) :-
	get_record(RecordName, Fields, 1),
	set_record(RecordName, Fields, 1).
	

swap_place(New,[_Head|Tail],0,Acc) :-
	Acc = [New|Tail].
swap_place(New,[Head|Tail],N,Acc) :-
	Next is N - 1,
	Acc = [Head|R],
	swap_place(New, Tail, Next, R).

set_record(_, [], _) :- !.
set_record(RecordName, [Field|Rest], Place) :-
	SetRule =.. [RecordName, Field, Record, NewValue, NData],	
	N is Place + 1,
	set_record(RecordName, Rest, N),
	asserta((SetRule :-
		Record    =.. Data,
		 Pivot is N - 1,
		 swap_place(NewValue,Data, Pivot,NewRecord),
		 NData =.. NewRecord

		)).

	
get_record(_, [], _) :-!.
get_record(RecordName, [Field|Rest], Place) :-
	GetRule =.. [RecordName, Field, Record, Value],
	asserta((GetRule :- arg(Place, Record, Value))),
	N is Place + 1,
	get_record(RecordName, Rest, N).

