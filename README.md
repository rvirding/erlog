Erlog is a Prolog interpreter implemented in Erlang and integrated
with the Erlang runtime system. It is a subset of the Prolog standard.
An erlog shell is also included.

### Usage
Make erlog:  

    make rel

#### Command line prolog coding:
Run release:

    ./rel/erlog/bin/erlog start
And connect to it via console:

    telnet 127.0.0.1 8080

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
    <<"Yes">>
    (erlog@127.0.0.1)3> erlog:execute(Pid, "father('victor', 'andrey').").        
    <<"Yes">>
    (erlog@127.0.0.1)4> erlog:execute(Pid, "father('victor', 'vasya')."). 
    <<"No">>

#### Custom database server:
Erlog now supports using your own database, instead of using ets and dicts. Just implement `erlog_storage` callback interface
and pass your module name with your implementation to `erlog:start_link/1`.  
Example:  
    
    erlog:start_link(mysql_storage_impl_module).