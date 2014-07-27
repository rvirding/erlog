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
                E = erlog:new(),
                Fields                                  = record_info(fields, person),
                {ok, E1}                                = E({consult,"../priv/records.pl"}),

                {{succeed,_}, E2}                       = E1({prove, {record, person, Fields}}),

                {{succeed,[{'Name', Name}]}, _ }        = E2({prove, {person, name, Person, {'Name'}}}),
                ?assertEqual(Person#person.name, Name),
                {{succeed,[{'Phone', Phone}]}, _}       = E2({prove, {person, phone, Person, {'Phone'}}}),
                ?assertEqual(Person#person.phone, Phone),
                {{succeed,[{'Address', Address}]}, _}   = E2({prove, {person, address, Person, {'Address'}}}),
                ?assertEqual(Person#person.address, Address),
                {{succeed,[{'Comments', Comments}]}, _} = E2({prove, {person, comments, Person, {'Comments'}}}),
                ?assertEqual(Person#person.comments, Comments),
                true
	    end).
 
prop_prolog_records_set() ->
    ?FORALL({Person,NewName},
	    {person(),name()},
	    begin
                E = erlog:new(),
                {ok, E1}                                = E({consult,"../priv/records.pl"}),
                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = E1({prove, {record, person, Fields}}),

		{{succeed,[{'Person', NewPerson }]},_} =
		    E2({prove,{person, name, Person, NewName, {'Person'}}}),
		?assert(is_record(NewPerson, person)),
		?assertEqual(NewPerson#person.name , NewName),

		{{succeed,[{'Person', NewPerson1 }]},_} =
		    E2({prove,{person, address, Person, NewName, {'Person'}}}),
		?assertEqual(NewPerson1#person.address , NewName),
		true
	    end).
