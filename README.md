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
File consulter is a module, used to operate with files. It's behaviour is described in `erlog_file_consulter`. It should
implement two functions: `lookup` and `load`. Lookup returns list of all prolog libraries from selected directory and load
reads selected file and parse it to prolog terms.  
Default implementation use files and directories for libraries search and loading. If you implement your own file consulter,
f.e. if you use database filesystem or smth else - implement `erlog_file_consulter` behaviour in your module and pass its
name in erlog configuration as __f_consulter__: 

    ConfList = [{f_consulter, my_hadoop_consulter}],
    erlog:start_link(ConfList).
    
#### Custom debugger handler:
If you wan't to use functions from debug library - you should define your own gen_event handler and pass it to erlog.
All debug events from such debug functions as `writeln/1` will be passed there.  
See `erlog_simple_printer` as a default implementation of console printer as an example, or `erlog_remote_eh`, which is 
intended to print debug to remote client.  
To configure your gen_event module - just pass module and arguments as __event_h__ in configuration:

    ConfList = [{event_h, {my_event_handler, Args}}],
    erlog:start_link(ConfList).
    
#### Working with libraries:
Erlog supports two kinds of libraries: native (written in Erlang) and extended (written in Prolog). Native libraries can 
be standard and external. 
All predicates from standard libraries are loaded to memory when you start erlog core.    
All prolog libraries from `lib/autoload` are also loaded to memory when you start erlog core.
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
##### Automatic libraries loading
For convenient libraries usage you can load all libraries you need when creating a core. It will let you not to call `use/1`
everywhere in your code. Just add param `{libraries, [my_first_lib, my second_lib]}` in your params when starting a core:

    ConfList = [{libraries, [Lib1, Lib2]}],
    erlog:start_link(ConfList).
All libraries from array will be loaded.
More in [docs](https://github.com/comtihon/erlog/blob/master/doc/libraries.md "libraries").  
##### Loading Prolog libraries
When configuring erlog you should set default library directory as __libs_dir__:
    
    ConfList = [{libs_dir, "/usr/share/prolog/lib/"}],
    erlog:start_link(ConfList).
If you don't set this - erlog will use `../lib` directory, assuming it was run from `ebin`.   
For manual loading prolog library - also try `use`, but instead of __atom__ name call it with __string__ library name:

    use(erlog_cache). %use extended native library
    use("proc/cuda/driver.pl"). %use prolog library, from /usr/share/prolog/lib/proc/cuda/
__Important!__ To avoid `erlog_parse,{operator_expected,'.'}` error - sure, that last character in your prolog file is `\n`.