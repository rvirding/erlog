Erlog is a Prolog interpreter implemented in Erlang and integrated
with the Erlang runtime system. It is a subset of the Prolog standard.
An erlog shell is also included.

### Usage
Make erlog:  

    make rel

#### Command line prolog coding:
##### Local shell
To use local shell just run erlang emulator in directory with compiled beam files (`ebin`) and run local shell:

    make
    cd ebin
    erl
    erlog_local_shell:start().
##### Remote shell
You can use remote shell and connect to it via telnet.
Run release:

    ./rel/erlog/bin/erlog start
And connect to it via console:

    telnet 127.0.0.1 8080
Port can be set up in `src/erlog.app.src`. 

#### Debugger
Debugger can be passed to erlog as a parameter {debugger, Fun}, where `Fun` is your fun of calling debugger:

    {ok, Core} = erlog:start_link([{debugger, fun(Status, Functor, Result) -> gen_server:call(Debugger, {Status, Functor, Result}) end}]),
Where __Status__ is a status of command - `ok|failed`, __Functor__ is current working functor, __Result__ is a result 
prolog term - complex structure with all data.  
As an example you can use `erlog_simple_debugger` with `erlog_local_shell`:

    {ok, Pid} = erlog_simple_debugger:start_link().
    erlog_local_shell:start(Pid).
More in [docs](https://github.com/comtihon/erlog/blob/master/doc/debugger.md "debugger").  

#### Processing prolog code from erlang:
##### Starting
Spawn new logic core: 

    {ok, Pid} = erlog:start_link().
##### Executing
Process prolog terms, using your core:

    erlog:execute(Worker, Command).
Where:  
`Command` is a command, ended with dot,  
`Worker` is a pid of your prolog logic core. 
##### Selecting
When erlog:execute returns `select` in result - you can select for some other result calling `erlog:select/2` instead of execute.

    erlog:select(Worker, ";").
Full Example:

    (erlog@127.0.0.1)1> {ok, Pid} = erlog:start_link().
    {ok,<0.961.0>}
    (erlog@127.0.0.1)2> erlog:execute(Pid, "assert(father('victor', 'andrey')).").
    true
    (erlog@127.0.0.1)3> erlog:execute(Pid, "father('victor', 'andrey').").        
    true
    (erlog@127.0.0.1)4> erlog:execute(Pid, "father('victor', 'vasya')."). 
    false
    (erlog@127.0.0.1)5> erlog:execute(Pid, "run(S)."). 
    {{true,[{'S',600}]}, select}
    (erlog@127.0.0.1)6> erlog:select(Pid, ";"). 
    false

#### Custom database server:
Erlog now supports using your own database, instead of using ets and dicts. Just implement `erlog_storage` callback interface
and pass your module name with your implementation to `erlog:start_link/1` as __database__ to configuration list.  
Example:  
    
    ConfList = [{database, mysql_storage_impl_module}],
    erlog:start_link(ConfList).
You can pass your parameters to your database implementation:

    ConfList = [{database, dbModule}, {arguments, Params}],
    erlog:start_link(ConfList).
Where `Params` is a list of your args, need to be passed to `dbModule:new/1` function.

#### Consulting files
To consult files use brakes and filename with path `["/home/prolog_user/prolog_code/examples/family.pl"]`.  
Erlog also supports calling `consult/1` and `reconsult/1` from prolog code:  

    erlog:execute(Pid, "consult(\"/home/prolog_user/prolog_code/examples/family.pl\")."). 
__Remember!__ For proper consulting files with default consulter, files should end with empty line!  

#### Custom file consulter:
Basic file consulting takes `FileName` as argument and loads file from your filesystem.  
But if your production-system needs to consult files from database, of shared filesystem, or something else - you can create
your own function for consulting files and pass it to erlog.  
Just add your function to configuration list as __f_consulter__:

    F = fun(Filename) -> my_hadoop_server:get_file(Filename) end,
    ConfList = [{f_consulter, F}],
    erlog:start_link(ConfList).
    
#### Custom debugger handler:
If you wan't to use functions from debug library - you should define your own gen_event handler and pass it to erlog.
All debug events from such debug functions as `writeln/1` will be passed there.  
See `erlog_simple_printer` as a default implementation of console printer as an example, or `erlog_remote_eh`, which is intended to print debug to remote client.  
To configure your gen_event module - just pass module and arguments as __event_h__ in configuration:

    ConfList = [{event_h, {my_event_handler, Args}}],
    erlog:start_link(ConfList).
    
#### Working with libraries:
Erlog is implemented in erlang modules, called libraries. They can be standard and external. 
All predicates from standard functions are loaded to memory when you start erlog core.  
##### Manual loading external libraries
But to use predicates from external functions - you should manually load them to memory with the help of `use/1` command:

    | ?- db_assert(test,foo(a,b)).
    false
    | ?- use(erlog_db).
    true
    | ?- db_assert(test,foo(a,b)).
    true
This example demonstrates the loading of external database library.  
First call is false, because there is no such function loaded to memory.   
Second - library is loaded.  
Third - function run successfully.  
__Important!__ If you are working with erlog from poolboy or dynamic creating erlog gen_servers through supervisor, 
remember, that two execution requests can be processed on different erlog instance.  

    use(some_lib). %returns true
    some_lib_fun(some_val). %returns false
In this example system erlog gen server is created one per one separate command (F.e. http request). Firstly - library
`some_lib` is loaded. Than erlog server with loaded library is destroyed (as request is complete) and for another request
`some_lib_fun(some_val)` another erlog server is created, but, without loaded library.  
More in [docs](https://github.com/comtihon/erlog/blob/master/doc/libraries.md "libraries").  