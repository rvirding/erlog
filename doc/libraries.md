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

### Library autoload
For convenient libraries usage you can load all libraries you need when creating a core. It will let you not to call `use/1`
everywhere in your code. Just add param `{libraries, [my_first_lib, my second_lib]}` in your params when starting a core:

    ConfList = [{libraries, [my_first_lib, my second_lib]}],
    erlog:start_link(ConfList).
All libraries from array will be loaded.
    
### Writing your own libraries
You can write your own external libraries. For doing so - just setup behaviour `erlog_exlib`. It has one callback function
`load(Db)` for initialisation library. Then you should define your execution functions. See __External libraries__ for 
instructions of library execution functions format.  
Example:  
_File `erlog_uid.hrl`_
    
    -define(ERLOG_UID,
	[
		{{id, 1}, ?MODULE, id_1}
	]).
_File `erlog_uid.erl`_	
	
	-behaviour(erlog_exlib).
    -include("ep_uuid.hrl").
    -include("erlog_core.hrl").
    
    -export([load/1, id_1/1]).
    
    load(Db) ->
	    lists:foreach(fun(Proc) -> erlog_memory:load_library_space(Db, Proc) end, ?ERLOG_UID).

    id_1(Params = #param{goal = {id, Res}, next_goal = Next, bindings = Bs0}) ->
	    Bs = ec_support:add_binding(Res, binary_to_list(uuid:generate()), Bs0),
	    ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).
	    
### Prolog libraries
You can also write prolog libraries, which you can load manually of automatically. All this libraries will be loaded to 
library space. For automatic loading libraries - move them to `lib/autoload` directory.  
Note, that if you create a functor in prolog library and load this library - you won't create same functor in userspace with
the help of `assert`. Also - if you have same functors in different libraries - they will be rewritten by last loaded.