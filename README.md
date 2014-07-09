
[![Build Status](https://travis-ci.org/zkessin/erlog.svg?branch=master)](https://travis-ci.org/zkessin/erlog)

## Erlog - Prolog for an Erlang Application

Erlog is a Prolog interpreter implemented in Erlang and integrated
with the Erlang runtime system. It is a subset of the Prolog standard.
An erlog shell (REPL) is also included.

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

## The Fuction interface

To create an erlog instance in a closure use +erlog:new()+ this will
return a function that can be invoked to run an erlog program. To
prove a clause you can then run _E({prove, ...})_ This will return a
new closure and a return of type _erlog_return()_. To consult you can
run _E({consult,FILE})_ which will return a new closure and 'ok' or an
error.

For example take this code:
We start by creating a new instance of the erlog engine, then we 
it starts with an append statement which ask it to append lists *A*
and *B*. The return value is designated with a 1 tuple with an atom
value for the return variable, in this case *{'Z'}*. 

If the prolog code works correctly it will return the tuple {{succeed,
[{'Z', Value}]}, E1}. Here E1 is the new state of the erlog interpreter.



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

If you want to setup erlog in its own server then you can use the
command _erlog:start()_ or _erlog:start_link()_ from there you can
load files with _erlog:consult(PID, FILE)_ and run code with
_erlog:prove(PID,{...})_. You can also provide the prolog code in a
string or as a pre-compiled tuple.



The thing to note with this interface is that the erlog process can be
accessed from any process that knows about the server, so it is
possible to have strange concurrency errors, for example with the
_erlog:next_solution/1_ function.   

## Using ETS

Erlog can also share data with an erlang program by way of an ETS
table. Erlog includes commands to unify a goal with the contents of an
ets table. It should also be possible to work with mnesia tables, but
this has not yet been done.

If you want to use erlog with ets you need to load the erlog_ets
module into erlog. To do that you call _erlog:load(PID,erlog_ets)_ or
_E({load,erlog_ets})_. You can match on an ets table with
_ets_match(TableId, Value)_.


## Including with rebar

You can include erlog in your application with rebar, by adding it to
the deps section of your rebar config file.

## Testing

Erlog is tested to work with Erlang versions R14B02 - 17, the tests
are quick-check properties, if you do not have quickcheck don't worry
you can still use erlog, you just won't be able to run the
properties. 

If you want to run the tests you will need to install quickcheck mini
(Or the full quickcheck) you can do this with these commands:

````bash
   wget http://www.quviq.com/downloads/eqcmini.zip
   unzip eqcmini.zip
   export ERL_LIBS=eqcmini:$ERL_LIBS
````

to run the tests then run _rebar qc_

## Licence 

Erlog was created by Robert Virding and is can be used under the
Apache 2.0 Licence. 

