-module(records_test).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).


-record(person, {name, phone, address, comments}).

person() ->
    #person{name	= elements(["Adam", "Bob","Charlie"]),
	    phone	= vector(8, choose(0,9)),
	    address	= list(char()),
	    comments	= binary()}.


prop_prolog_records() ->
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
 
