-module(records_test).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).


-record(person, {name, phone, address, comments}).
name() ->
    elements(["Adam", "Bob", "Charlie"]).

person() ->
    #person{name	= name(),
	    phone	= vector(1, choose(48,57)),
	    address	= list(char()),
	    comments	= binary()}.


prop_prolog_records_get() ->
    ?FORALL(Person,
	    person(),
	    begin
		application:set_env(erlog, consult_path, [".", "stdlib"]),
                {ok,E}					= erlog:new(),
                {ok, E1}                                = erlog:consult("erlang.pl", E),
                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = erlog:prove({record, person, Fields}, E1),


                {{succeed,[{'Name', Name}]}, _ }        = erlog:prove({person, name, Person, {'Name'}}, E2),
                ?assertEqual(Person#person.name, Name),
                {{succeed,[{'Phone', Phone}]}, _}       = erlog:prove({person, phone, Person, {'Phone'}}, E2),
                ?assertEqual(Person#person.phone, Phone),
                {{succeed,[{'Address', Address}]}, _}   = erlog:prove({person, address, Person, {'Address'}}, E2),
                ?assertEqual(Person#person.address, Address),
                {{succeed,[{'Comments', Comments}]}, _} = erlog:prove({person, comments, Person, {'Comments'}}, E2),
                ?assertEqual(Person#person.comments, Comments),
                true
	    end).

prop_prolog_records_set() ->
    ?FORALL({Person,NewName},
	    {person(),name()},
	    begin
                {ok,E} = erlog:new(),
                {ok, E1}                                = erlog:consult("stdlib/erlang.pl", E),
                Fields                                  = record_info(fields, person),
                {{succeed,_}, E2}                       = erlog:prove({record, person, Fields}, E1),

		{{succeed,[{'Person', NewPerson }]},_} =
		    erlog:prove({person, name, Person, NewName, {'Person'}}, E2),
		?assert(is_record(NewPerson, person)),
		?assertEqual(NewPerson#person.name , NewName),

		{{succeed,[{'Person', NewPerson1 }]},_} =
		    erlog:prove({person, address, Person, NewName, {'Person'}}, E2),
		?assertEqual(NewPerson1#person.address , NewName),
		true
	    end).
