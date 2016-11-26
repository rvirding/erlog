
[![Build Status](https://travis-ci.org/zkessin/erlog.svg?branch=master)](https://travis-ci.org/zkessin/erlog)

## Erlog - Prolog for an Erlang Application

Erlog is a Prolog interpreter implemented in Erlang and integrated
with the Erlang runtime system. It is a subset of the Prolog standard.
An Erlog shell (REPL) is also included.

You should use this if you want to include some Prolog or logic
programming functionality in a larger Erlang system (Including Elixir,
LFE, Joxa etc). If you want a stand alone Prolog you are probably
better off using a package like SWI Prolog.


## The Function interface

This is a low level interface, which is meant to built upon as much as used directly.

To create an Erlog instance in a closure use `erlog:new()` this will
return `{ok, State}` Where state is the current state of the Erlog
system. You should treat it as an opaque data structure. To prove a
clause or run Prolog code you can then run `erlog:prove(State, {...})`
This will return a new closure and a return of type
`erlog_return()`. To consult a file you can run `erlog:consult(State,
FILE)` which will return a new closure and 'ok' or an error.

For example take this code:
We start by creating a new instance of the Erlog engine, then we 
it starts with an append statement which ask it to append lists `A`
and `B`. The return value is designated with a 1 tuple with an atom
value for the return variable, in this case `{'Z'}`. 

If the Prolog code works correctly it will return the tuple `{{succeed,
[{'Z', Value}]}, NewState}`. 



````erlang
           {ok,Erlog}         = erlog:new(),
           case  erlog:prove({append,A,B,{'Z'}}, Erlog) of
               {{succeed, [{'Z', Z}]}, E1} when is_record(E1,est) ->
                   Z =:= lists:append(A,B);
               fail ->
                   false
           end
````

The dialyzer types of some of Erlog's functions are as such

````erlang
-opaque erlog_state()			:: #est{}.
-type functor()                 :: tuple().
-type erlog_return(Value)		:: {Value,erlog_state()}.
-spec prove(erlog_state(), functor()) -> erlog_return({succeed, [{atom(), any()}]}|fail).
-spec prove(erlog_state(), file()) -> erlog_return(ok|{error, atom()}).


````

If you want to build a gen_server out of your Prolog code checkout the Erlog server project https://github.com/zkessin/erlog-server

If you have questions about Erlog post them tagged with Erlog on Stack Overflow http://stackoverflow.com/questions/tagged/erlog

## Passing Data between Erlang and Prolog

If you want to pass data between Erlang and Prolog it is pretty easy
to do so. Data types map pretty cleanly between the two languages due
to the fact that Erlang evolved from Prolog. 

### Atoms
Atoms are the same in Erlang and Prolog, and can be passed back and
forth without problem.

### Numeric Data 
Integer and floating point numbers similarly can be passed back and
forth. 

### Opaque data

Erlog does not understand references, ports and pids. They can be
passed threw Erlog but Erlog won't be able to do more than basic
comparisons on them.

### Structured Data

It is possible to send structured Erlang data to Prolog, and this is
often very useful. Lists can be sent directly back and forth. Maps are
not (Yet) supported, we will be looking into how to support them in
the future. 

Erlog understands Erlang tuples to be facts. So the Erlang tuple
`{foo, 1, 2, 3}` would show up in Erlog as the fact `foo(1,2,3)`. The
upshot of this is that all tuples that are passed to Erlog must have
an atom as the first element and must have more than 1 element. The
tuple `{atom()}` will be understood to be a Prolog variable. 

Records in Erlang are just tuples with an initial atom. So it is
possible to pass records between Erlog and Erlang. The record
definition here and the Prolog fact are equivalent. 

````erlang
-record(person, {name, phone, address}).
````

````prolog
person(Name, Phone, Address).
````

You can access fields in an Erlang record by position by using the
standard prolog arg/3 predicate.  If you want to create functors that
can access fields in an Erlang record by name, you can create functors
for that Automaticly with the code in the file
https://github.com/zkessin/erlog/blob/master/priv/records.pl. just
call `erlog:prove(State, {record, person, record_info(fields,
person)})`. Note that the record fields must be created in Erlang at
compile time.

## Using ETS

Erlog can also share data with an Erlang program by way of an ETS
table. Erlog includes commands to unify a goal with the contents of an
ETS table. It should also be possible to work with mnesia tables, but
this has not yet been done.

If you want to use Erlog with ETS you need to load the erlog_ets
module into Erlog. To do that you call `erlog:load(PID,erlog_ets)` or
`E({load,erlog_ets})`. You can match on an ETS table with
`ets_match(TableId, Value)`.

## Including with rebar

You can include Erlog in your application with rebar, by adding it to
the deps section of your rebar config file.

## Testing

Erlog is tested to work with Erlang versions R14B02 - 17, the tests
are both eunit tests and quick-check properties, if you do not have
quickcheck don't worry you can still use Erlog, you just won't be able
to run the properties.

If you want to run the tests you will need to install quickcheck mini
(Or the full quickcheck) you can do this with these commands:

````bash
   wget http://www.quviq.com/downloads/eqcmini.zip
   unzip eqcmini.zip
   export ERL_LIBS=eqcmini:$ERL_LIBS
````

to run the tests then run `rebar eunit`

## Licence 

Erlog was created by Robert Virding and can be used under the
Apache 2.0 Licence. 

