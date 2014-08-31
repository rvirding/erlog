% -*- mode: prolog -*-
%% Copyright (c) 2014 Zachary Kessin
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Code for handling erlang records
%
% erlog:prove(State, {record, RecordName, record_info(fields,RecordName)})
% will define erlog predicates to access that record by field name
% and to modify them
%
% person(Field, Record, Value).
% person(Field, Record, NewValue, NewRecord).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

