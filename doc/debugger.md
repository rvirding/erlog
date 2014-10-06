### Using debugger
Debugger is started in `listing` mode by default. Listing just log every goal call and memory after previous call. For
more efficient usage - stop points can be added. 

#### Stopping with counter
To stop commands execution after N executed commands - configure debugger with `next N`, where N is a positive integer.
When N goals is skipped - you will be asked again. You can configure the action by hitting C, or skipping next N goals 
by pressing `enter`.    
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
    [C_]:C
    Select action

#### Stopping with goal
To stop commands execution after special command (breakpoint) - configure debugger with `stop G`, where G is a goal. Goal
can be set direct - with params, f.e. `foo(a,b)` - it will react only on this goal, or indirect - by arity `foo/2` - it
will react on every foo with arity 2. When such goal will be executed - debugger will stop code execution and ask you for
next configuration. If you want to skip all code up to an end - just use `listing`.  
Example:

    1> {ok, Pid} = erlog_simple_debugger:start_link().
    {ok,<0.34.0>}
    2> erlog_simple_debugger:configure(Pid).
    Select action
    | ?- stop assert(foo(a,b))
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
    
#### Spying for goals
To debug only for selected goal - use `spy G`, where G is a goal. Goal can be set by arity or direct, same as in stop 
section. When goal matches - debugger will print execution, otherwise it will be silent.

    1> {ok, Pid} = erlog_simple_debugger:start_link().
    {ok,<0.34.0>}
    2>  erlog_simple_debugger:configure(Pid).
    Select action
    | ?- spy foo/2
    ok
    3>  erlog_local_shell:start(Pid).
    Erlog Shell V6.1 with debugger (abort with ^G)
    | ?- assert(foo(a,b)), assert(foo(a,c)), foo(a,b), assert(foo(a,c)), foo(a,c), assert(foo(a,d)),foo(a,d), assert(foo(a,b)).
    Execute {foo,a,b}, memory: []
    true
    