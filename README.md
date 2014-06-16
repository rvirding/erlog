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

    Logic = erlog_core:new().
Process prolog terms, using your core:

    erlog:process_command(CommandRaw, Spike, Core).
Where `CommandRaw` is a command, ended with dot, `Spike` is used for selecting variants of solutions, 
`Core` is a pid of your prolog logic core.

#### Calling erlang functions from erlang:
Spawn new logic core: 

    Logic = erlog_core:new().
Use functions from `erlog_core`: prove/2, next_solution/1, consult/2, reconsult/2, get_db/1, set_db/2, halt/1 then.