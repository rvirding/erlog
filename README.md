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

#### Processing prolog code from erlang:
Spawn new logic core: 

    {ok, Pid} = erlog:start_link().
Process prolog terms, using your core:

    erlog:execute(Worker, Command).
Where:  
`Command` is a command, ended with dot,  
`Worker` is a pid of your prolog logic core.  
Full Example:

    (erlog@127.0.0.1)1> {ok, Pid} = erlog:start_link().
    {ok,<0.961.0>}
    (erlog@127.0.0.1)2> erlog:execute(Pid, "assert(father('victor', 'andrey')).").
    true
    (erlog@127.0.0.1)3> erlog:execute(Pid, "father('victor', 'andrey').").        
    true
    (erlog@127.0.0.1)4> erlog:execute(Pid, "father('victor', 'vasya')."). 
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

    ConfList = [{event_h, {my_event_handler, Args}],
    erlog:start_link(ConfList).