### Using debugger
Debugger is started in `listing` mode by default. Listing just log every goal call and memory after previous call. For
more efficient usage - stop points can be added. 

#### Stopping with counter
To stop commands execution after N executed commands - configure debugger with `next N`, where N is a positive integer.
After N goals code execution will be stopped and you will be asked again to configure debugger.  
Example:

    1> {ok, Pid} = erlog_simple_debugger:start_link().
    {ok,<0.34.0>}
    2>  erlog_simple_debugger:configure(Pid).
    Select action
    | ?- next 5
    ok
    3> erlog_local_shell:start(Pid).
    Erlog Shell V6.1 with debugger (abort with ^G)ert(foo(a,b)), foo(a,b), writeln("world").
                                                      Skip {call,{',',{assert,{foo,a,c}},
    | ?- assert(foo(a,c)), writeln("hello"), retract(foo(a,c)),  ass
                    {',',{writeln,"hello"},
                         {',',{retract,{foo,a,c}},
                              {',',{assert,{foo,a,b}},
                                   {',',{foo,a,b},{writeln,"world"}}}}}}}
    Skip {assert,{foo,a,c}}
    Skip {writeln,"hello"}
    Skip {retract,{foo,a,c}}
    Erlog debugger stopped execution on command {assert,{foo,a,b}} with memory: [].
    Select action

#### Stopping with goal
To stop commands execution after special command (breakpoint) - configure debugger with `stop G`, where G is a prolog term.
When such goal will be executed - debugger will stop code execution and ask you for next configuration. If you want to skip
all code up to an end - just use `listing`.  
Example:

    1> {ok, Pid} = erlog_simple_debugger:start_link().
    {ok,<0.34.0>}
    2> erlog_simple_debugger:configure(Pid).
    Select action
    | ?- stop {assert,{foo,a,b}}
    ok
    3> erlog_local_shell:start(Pid).
    Erlog Shell V6.1 with debugger (abort with ^G)
    | ?-  assert(foo(a,c)), writeln("hello"), retract(foo(a,c)),  assert(foo(a,b)), foo(a,b), writeln("world"). 
    Skip {call,{',',{assert,{foo,a,c}},
               {',',{writeln,"hello"},
                    {',',{retract,{foo,a,c}},
                         {',',{assert,{foo,a,b}},
                              {',',{foo,a,b},{writeln,"world"}}}}}}}
    Skip {assert,{foo,a,c}}
    Skip {writeln,"hello"}
    Skip {retract,{foo,a,c}}
    Erlog debugger stopped execution on command {assert,{foo,a,b}} with memory: [].
    Select action