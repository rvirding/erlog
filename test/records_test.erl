-module(records_test).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).


-record(person, {name, phone, address, comments}).
name() ->
    elements(["Adam", "Bob","Charlie"]).

person() ->
    #person{name	= name(),
	    phone	= vector(1, choose(48,57)),
	    address	= list(char()),
	    comments	= binary()}.


prop_prolog_records_get() ->
    ?FORALL(Person,
	    person(),
	    begin
                {ok,E}					= erlog:new(),
                {ok, E1}                                = erlog:consult(E,"../priv/records.pl"),
                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = erlog:prove(E1, {record, person, Fields}),


                {{succeed,[{'Name', Name}]}, _ }        = erlog:prove(E2, {person, name, Person, {'Name'}}),
                ?assertEqual(Person#person.name, Name),
                {{succeed,[{'Phone', Phone}]}, _}       = erlog:prove(E2, {person, phone, Person, {'Phone'}}),
                ?assertEqual(Person#person.phone, Phone),
                {{succeed,[{'Address', Address}]}, _}   = erlog:prove(E2, {person, address, Person, {'Address'}}),
                ?assertEqual(Person#person.address, Address),
                {{succeed,[{'Comments', Comments}]}, _} = erlog:prove(E2, {person, comments, Person, {'Comments'}}),
                ?assertEqual(Person#person.comments, Comments),
                true
	    end).
 
prop_prolog_records_set() ->
    ?FORALL({Person,NewName},
	    {person(),name()},
	    begin
                {ok,E} = erlog:new(),
                {ok, E1}                                = erlog:consult(E,"../priv/records.pl"),
                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = erlog:prove(E1,{record, person, Fields}),

		{{succeed,[{'Person', NewPerson }]},_} =
		    erlog:prove(E2,{person, name, Person, NewName, {'Person'}}),
		?assert(is_record(NewPerson, person)),
		?assertEqual(NewPerson#person.name , NewName),

		{{succeed,[{'Person', NewPerson1 }]},_} =
		    erlog:prove(E2,{person, address, Person, NewName, {'Person'}}),
		?assertEqual(NewPerson1#person.address , NewName),
		true
	    end).
