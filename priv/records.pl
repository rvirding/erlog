%-*- Prolog -*-


record(_,[]):- !.
record(RecordName,Fields) :-
	record(RecordName, Fields,1).

record(_, [], _) :-!.
record(RecordName, [Field|Rest], Place) :-
	Rule =.. [RecordName,Field,Record,Value],
	asserta((Rule :- arg(Place, Record, Value))),
	N is Place + 1,
	record(RecordName, Rest, N).
