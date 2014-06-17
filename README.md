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