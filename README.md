
[![Build Status](https://travis-ci.org/zkessin/erlog.svg?branch=master)](https://travis-ci.org/zkessin/erlog)

## Erlog - Prolog for an Erlang Application

Erlog is a Prolog interpreter implemented in Erlang and integrated
with the Erlang runtime system. It is a subset of the Prolog standard.
An Erlog shell (REPL) is also included.

with the Erlang runtime system, that runs in an Erlang process. It is
a subset of the ISO Prolog standard.

You should use this if you want to include some prolog functionality
in a larger erlang system (Including Elixir, LFE, Joxa etc). If you
want a stand alone prolog you are probably better off using a package
like SWI Prolog.

There are currently 3 ways of interacting with Erlog, you can use the
Erlog REPL for ad hoc testing, you can create an Erlog implementation
in a closure or you can create an Erlog instance in a
gen_server. Which version you should use depends on your application.

## The Function interface

This is a low level interface, which is ment to built upon as much as used directly.

To create an Erlog instance in a closure use `erlog:new()` this will
return a function that can be invoked to run an Erlog program. To
prove a clause you can then run `E({prove, ...})` This will return a
new closure and a return of type `erlog_return()`. To consult you can
run `E({consult,FILE})` which will return a new closure and 'ok' or an
error.

For example take this code:
We start by creating a new instance of the Erlog engine, then we 
it starts with an append statement which ask it to append lists `A`
and `B`. The return value is designated with a 1 tuple with an atom
value for the return variable, in this case `{'Z'}`. 

If the prolog code works correctly it will return the tuple `{{succeed,
[{'Z', Value}]}, E1}`. Here E1 is the new state of the Erlog interpreter.



````erlang
           E         = erlog:new(),
           case  E({prove,{append,A,B,{'Z'}}}) of
               {{succeed, [{'Z', Z}]}, E1} when is_function(E1) ->
                   Z =:= lists:append(A,B);
               fail ->
                   false
           end
````

Erlog prove has a type signature like this:

````erlang
-type erlog_return() :: fail|{succeed, [{atom(), any()}]}.
````
## The gen_server interface

*NOTE THIS INTERFACE MAY GO AWAY*

If you want to setup Erlog in its own server then you can use the
command `erlog:start()` or `erlog:start_link()` from there you can
load files with `erlog:consult(PID, FILE)` and run code with
`erlog:prove(PID,{...})`. You can also provide the prolog code in a
string or as a pre-compiled tuple.



The thing to note with this interface is that the Erlog process can be
accessed from any process that knows about the server, so it is
possible to have strange concurrency errors, for example with the
`erlog:next_solution/1` function.

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

It is possible to send structured erlang data to prolog, and this is
often very useful. Lists can be sent directly back and forth. Maps are
not (Yet) supported, we will be looking into how to support them in
the future. 

Erlog understands Erlang tuples to be facts. So the erlang tuple
`{foo, 1, 2, 3}` would show up in Erlog as the fact `foo(1,2,3)`. The
upshot of this is that all tuples that are passed to Erlog must have
an atom as the first element and must have more than 1 element. The
tuple `{atom()}` will be understood to be a prolog variable. 

Records in Erlang are just tuples with an initial atom. So it is
possible to pass records between Erlog and erlang. The record
definition here and the prolog fact are equivalent. 

````erlang
-record(person, {name, phone, address}).
````

````prolog
person(Name, Phone, Address).
````

## Using ETS

Erlog can also share data with an erlang program by way of an ETS
table. Erlog includes commands to unify a goal with the contents of an
ets table. It should also be possible to work with mnesia tables, but
this has not yet been done.

If you want to use Erlog with ets you need to load the erlog_ets
module into Erlog. To do that you call `erlog:load(PID,erlog_ets)` or
`E({load,erlog_ets})`. You can match on an ets table with
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

Erlog was created by Robert Virding and is can be used under the
Apache 2.0 Licence. 

