### Core libraries
They are standard `built_in` libraries:
 
 * `erlog_bips` - core built-in functions.  
 * `erlog_core` - basic language functions.  
 * `erlog_dcg` - DCG conversion and procedures.  
 * `erlog_lists` - standard lists support.  
 * `erlog_time` - date and time support library.  

All built-in libraries have same behaviour `erlog_stdlib`. They have `load/1` function and `prove_goal/1` function.  
In `load` function - all initialisation of library is made. Starting all needed services, parsing prolog functors, 
loading predicates to memory and what not.  
When `prove_goal` is called - `#param{}` record with working data is passed to it. Function for goal execution is 
searching through pattern matching.  
Core libraries are loaded into memory when erlog gen_server starts.

### External libraries
They are external, user-defined libraries. They should also act as `erlog_exlib` behaviour, with `load/1` function.
`load` function made initialisation of library - as core library. Instead of `prove_goal` function - library functions
are defined in `*.hrl` files as compiled:  

    {{Name, Arity}, Module, Function}
`Name` - is name of function in prolog,  
`Arity` - is the arity of prolog function,  
`Module` - is the erlang module, where processing function is defined,  
`Function` - is erlang processing function.  
External libraries are load into memory on demand, by calling `use(LibName)` function, where LibName is the name of the 
erlang module with exlib behaviour.