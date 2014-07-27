%-*- Prolog -*-


record(_,[]):- !.
record(RecordName,Fields) :-
	get_record(RecordName, Fields, 1),
	set_record(RecordName, Fields, 1).
	

set_record(_, [], _) :- !.
set_record(RecordName, [Field|Rest], Place) :-
	SetRule =.. [RecordName, Field, Record, NewValue, NewRecord],	
	N is Place + 1,
	set_record(RecordName, Rest, N),
	asserta((SetRule :-
		Record    =.. Data,
		 display(Data),
		 append(Prefix, [_|Suffix], Data),
		 display(NewValue),
		 Pivot is N - 1,
		 length(Prefix,Pivot),
		 append(Prefix, [NewValue|Suffix], NData),
		 display(NData),
		 NewRecord =.. NData
		)).

	
get_record(_, [], _) :-!.
get_record(RecordName, [Field|Rest], Place) :-
	GetRule =.. [RecordName, Field, Record, Value],
	asserta((GetRule :- arg(Place, Record, Value))),
	N is Place + 1,
	get_record(RecordName, Rest, N).

