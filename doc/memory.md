### Erlog memory
#### Memory structure
All prolog predicates, that operate with memory - call `erlog_memory` gen_server for data. But `erlog_memory` does not 
operate with data itself. It calls memory implementations, which behavior is described in `erlog_storage`.  
Erlog memory is divided into three layers - __core__, __extended__ and __userspace__.  
__Core__ memory contains all core library predicates. They are loaded there when erlog starts. It is implemented in dict,
which is stored in `erlog_memory` state.  
__Extended__ memory contains all extended library predicates. They are loaded there on demand, using `use` keyword or 
library autoload (see [docs](https://github.com/comtihon/erlog/blob/master/doc/libraries.md "libraries")) They are also 
implemented in dict and stored in `erlog_memory` state.  
__Userspace__ memory contains all facts and predicates - user loads himself. Its behavior is described in `erlog_storage`.
Module, which implements this behavior is stored in `erlog_memory`. By default `erlog_dict` is used, but you can select 
`erlog_ets` or use your own.  
`Erlog_memory` saves module, which implements memory, and its state. State is returned in `new` function and in functions,
which can probably modify it. See `erlog_storage` specification for details. State is passed to storage implementation on
every call.

#### Using cursors
When prolog code is working - it often comes to a situation - where clauses for fact are found. Sometimes clauses number is
very big and multilayer. That is for cursors are used. When you use `erlog_storage` implementation with database - you can
fetch first clause from database and return cursor with result as `{cursor, Cursor, result, Result}`. Cursor will be saved 
is erlog gen_server state and will fetch next clause from database if needed. If clause succeeded - cursor will be closed.  
__Note__: prefer using cursors to fetching all clauses.  

#### Writing your own implementation
To write your own memory implementation, based on your favourite database, or cache, or something else - implement 
`erlog_storage` behavior and pass your module name in arguments, when you start erlog (as mentioned in Readme).  
`new/1` is called automatically - when erlog starts. New should be used for initialisation of the database. New takes 
list as param which was sent in erlog arguments for database module.  
`close/2` is called automatically - when erlog deside to close the cursor, database state can be updated there.  
`next/2` is called automatically to fetch next clause value from database, database state can be updated there. Also, you
can update your cursor, if you hame some complex logic.   
`get_procedure/2` is called automatically - when erlog is looking for a predicate to execute. First it search stdlib, then
extended lib and then userspace. It is main erlog predicate. Note, that it is called on every execution and can be the main 
reason of loading the database.  
Other functions are called manually, when different prolog predicates, depending on them, are called.