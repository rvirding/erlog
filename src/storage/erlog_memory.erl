%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 21:48
%%%-------------------------------------------------------------------
-module(erlog_memory).
-author("tihon").

-include("erlog_core.hrl").

-behaviour(gen_server). %TODO do we really need gen_server here?

%% API
-export([start_link/1,
  start_link/2,
  load_native_library/2,
  load_extended_library/2,
  load_extended_library/3,
  assertz_clause/3,
  asserta_clause/3,
  retract_clause/3,
  abolish_clauses/2,
  get_procedure/2,
  get_procedure_type/2,
  get_interp_functors/1,
  assertz_clause/2,
  asserta_clause/2,
  finadll/2,
  raw_store/3,
  raw_fetch/2,
  raw_append/3,
  raw_erase/2,
  listing/2,
  next/2,
  close/2]).

-export([db_assertz_clause/3,
  db_assertz_clause/4,
  db_asserta_clause/4,
  db_asserta_clause/3,
  db_retract_clause/4,
  db_abolish_clauses/3,
  get_db_procedure/3,
  db_findall/3,
  db_listing/3,
  db_next/3]).

-export([load_kernel_space/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state,  %TODO move to erlog, remove this process as separate
{
  stdlib :: dict,  %kernel-space memory
  exlib :: dict, %library-space memory
  database :: atom(), % callback module for user-space memory
  in_mem :: dict, %integrated memory for findall operations
  state :: term() % callback state
}).

%%%===================================================================
%%% API
%%%===================================================================
%% kernelspace predicate loading
load_kernel_space(Database, Module, Functor) -> gen_server:call(Database, {load_kernel_space, {Module, Functor}}).

%% libraryspace predicate loading
load_native_library(Database, Proc) -> gen_server:call(Database, {load_native, Proc}).

%% add prolog functor to libraryspace
load_extended_library(Database, {':-', Head, Body}) -> load_extended_library(Database, Head, Body);
load_extended_library(Database, Head) -> load_extended_library(Database, Head, true).
load_extended_library(Database, Head, Body) -> gen_server:call(Database, {load_extended, {Head, Body}}).

%% userspace predicate loading
assertz_clause(Database, {':-', Head, Body}) -> assertz_clause(Database, Head, Body);
assertz_clause(Database, Head) -> assertz_clause(Database, Head, true).
assertz_clause(Database, Head, Body) -> gen_server:call(Database, {assertz_clause, {Head, Body}}).

db_assertz_clause(Database, Collection, {':-', Head, Body}) -> db_assertz_clause(Database, Collection, Head, Body);
db_assertz_clause(Database, Collection, Head) -> db_assertz_clause(Database, Collection, Head, true).
db_assertz_clause(Database, Collection, Head, Body) ->
  gen_server:call(Database, {db_assertz_clause, {Collection, Head, Body}}).

asserta_clause(Database, {':-', H, B}) -> asserta_clause(Database, H, B);
asserta_clause(Database, H) -> asserta_clause(Database, H, true).
asserta_clause(Database, Head, Body) -> gen_server:call(Database, {asserta_clause, {Head, Body}}).

db_asserta_clause(Database, Collection, {':-', H, B}) -> db_asserta_clause(Database, Collection, H, B);
db_asserta_clause(Database, Collection, H) -> db_asserta_clause(Database, Collection, H, true).
db_asserta_clause(Database, Collection, Head, Body) ->
  gen_server:call(Database, {db_asserta_clause, {Collection, Head, Body}}).

db_findall(Database, Collection, Fun) -> gen_server:call(Database, {db_findall, {Collection, Fun}}).
finadll(Database, Fun) -> gen_server:call(Database, {findall, Fun}).

next(Database, Cursor) -> gen_server:call(Database, {next, Cursor}).
db_next(Database, Cursor, Table) -> gen_server:call(Database, {db_next, {Cursor, Table}}).

retract_clause(Database, F, Ct) -> gen_server:call(Database, {retract_clause, {F, Ct}}).
db_retract_clause(Database, Collection, F, Ct) ->
  gen_server:call(Database, {db_retract_clause, {Collection, F, Ct}}).

abolish_clauses(Database, Func) -> gen_server:call(Database, {abolish_clauses, Func}).
db_abolish_clauses(Database, Collection, Func) ->
  gen_server:call(Database, {db_abolish_clauses, {Collection, Func}}).

get_procedure(Database, Func) -> gen_server:call(Database, {get_procedure, Func}).
get_db_procedure(Database, Collection, Func) -> gen_server:call(Database, {get_db_procedure, {Collection, Func}}).

get_procedure_type(Database, Func) -> gen_server:call(Database, {get_procedure_type, Func}).

get_interp_functors(Database) -> gen_server:call(Database, get_interp_functors).

raw_store(Database, Key, Value) -> gen_server:call(Database, {raw_store, {Key, Value}}).

raw_fetch(Database, Key) -> gen_server:call(Database, {raw_fetch, Key}).

raw_append(Database, Key, Value) -> gen_server:call(Database, {raw_append, {Key, Value}}).

raw_erase(Database, Key) -> gen_server:call(Database, {raw_erase, Key}).

listing(Database, Args) -> gen_server:call(Database, {listing, Args}).
db_listing(Database, Collection, Args) -> gen_server:call(Database, {db_listing, {Collection, Args}}).

close(Database, Cursor) -> gen_server:call(Database, {close, Cursor}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Database :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Database) ->
  gen_server:start_link(?MODULE, [Database], []).
-spec(start_link(Database :: atom(), Params :: list() | atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Database, undefined) ->
  start_link(Database);
start_link(Database, Params) ->
  gen_server:start_link(?MODULE, [Database, Params], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Database]) when is_atom(Database) ->
  {ok, State} = Database:new(),
  {ok, init_memory(#state{database = Database, state = State})};
init([Database, Params]) when is_atom(Database) ->
  {ok, State} = Database:new(Params),
  {ok, init_memory(#state{database = Database, state = State})}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},  %TODO refactor me, get rid of gen_server and its callbacks
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({load_kernel_space, {Module, Functor}}, _From, State = #state{stdlib = StdLib}) ->  %load kernel space into memory
  UStdlib = dict:store(Functor, {built_in, Module}, StdLib),
  {reply, ok, State#state{stdlib = UStdlib}};
handle_call({load_native, {Functor, M, F}}, _From, State = #state{stdlib = StdLib, exlib = ExLib}) ->  %load library space into memory
  check_immutable(StdLib, Functor),
  {reply, ok, State#state{exlib = dict:store(Functor, {code, {M, F}}, ExLib)}};
handle_call({load_extended, {H, _} = F}, _From, State = #state{stdlib = StdLib, exlib = ExLib}) ->  %load library space into memory
  check_immutable(StdLib, erlog_ec_support:functor(H)),
  {Res, UExLib} = erlog_dict:assertz_clause({StdLib, ExLib, ExLib}, F), %use erlog_dict module to assert library to exlib dict
  {reply, Res, State#state{exlib = UExLib}};
handle_call({raw_store, {Key, Value}}, _From, State = #state{in_mem = InMem}) ->  %findall store
  Umem = store(Key, Value, InMem),
  {reply, ok, State#state{in_mem = Umem}};
handle_call({raw_fetch, Key}, _From, State = #state{in_mem = InMem}) ->  %findall fetch
  Res = fetch(Key, InMem),
  {reply, Res, State};
handle_call({raw_append, {Key, AppendValue}}, _From, State = #state{in_mem = InMem}) ->  %findall append
  Value = fetch(Key, InMem),
  Umem = store(Key, lists:concat([Value, [AppendValue]]), InMem),
  {reply, ok, State#state{in_mem = Umem}};
handle_call({raw_erase, Key}, _From, State = #state{in_mem = InMem}) ->  %findall erase
  Umem = dict:erase(Key, InMem),
  {reply, ok, State#state{in_mem = Umem}};
handle_call({abolish_clauses, Func}, _From, State = #state{stdlib = StdLib}) ->  %call third-party db module
  check_immutable(StdLib, Func),
  check_abolish(abolish_clauses, Func, Func, State);
handle_call({db_abolish_clauses, {_, Func} = Params}, _From, State = #state{stdlib = StdLib}) ->  %call third-party db module
  check_immutable(StdLib, Func),  %abolishing fact from default memory need to be checked
  check_abolish(db_abolish_clauses, Func, Params, State);
handle_call({Fun, {F, _} = Params}, _From, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib})
  when Fun == asserta_clause; Fun == assertz_clause ->
  check_immutable(StdLib, erlog_ec_support:functor(F)),  %modifying fact in default memory need to be checked
  check_immutable(ExLib, erlog_ec_support:functor(F)),
  do_action(Db, Fun, {StdLib, ExLib, DbState}, Params, State);
handle_call({Fun, {_, H, _} = Params}, _From, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib})
  when Fun == db_asserta_clause; Fun == db_assertz_clause ->
  check_immutable(StdLib, erlog_ec_support:functor(H)),  %modifying fact in default memory need to be checked
  check_immutable(ExLib, erlog_ec_support:functor(H)),
  do_action(Db, Fun, {StdLib, ExLib, DbState}, Params, State);
handle_call({retract_clause, {Func, _} = Params}, _From, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib}) ->
  check_immutable(StdLib, Func),  %modifying fact in default memory need to be checked
  check_immutable(ExLib, Func),
  do_action(Db, retract_clause, {StdLib, ExLib, DbState}, Params, State);
handle_call({db_retract_clause, {_, Func, _} = Params}, _From, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib}) ->
  check_immutable(StdLib, Func),  %modifying fact in default memory need to be checked
  check_immutable(ExLib, Func),
  do_action(Db, db_retract_clause, {StdLib, ExLib, DbState}, Params, State);
handle_call({Fun, Cursor}, _From, State = #state{state = DbState, database = Db}) when Fun == next; Fun == db_next ->  %get next result by cursor
  {Res, UState} = Db:Fun(DbState, Cursor),
  Ans = case Res of
          {cursor, After, result, Result} -> {After, Result}; %got new (or same cursor) and result. Form and return
          [] -> {Cursor, []}  %no result got - return old cursor and empty result
        end,
  {reply, Ans, State#state{state = UState}};
handle_call({close, Cursor}, _From, State = #state{state = DbState, database = Db}) ->  %get next result by cursor
  {Res, UState} = Db:close(DbState, Cursor),
  {reply, Res, State#state{state = UState}};
handle_call({Fun, Params}, _From, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib}) ->  %call third-party db module
  do_action(Db, Fun, {StdLib, ExLib, DbState}, Params, State);
handle_call(Fun, _From, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib}) ->  %call third-party db module
  try
    {Res, NewState} = Db:Fun({StdLib, ExLib, DbState}),
    {reply, Res, State#state{state = NewState}}
  catch
    throw:E -> {reply, E, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(halt, State) ->
  {stop, normal, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
%% Initialises three dicts for kernel, library memory and in_memory for findall operations
-spec init_memory(State :: #state{}) -> UpdState :: #state{}.
init_memory(State) ->
  D = dict:new(),
  State#state{stdlib = D, exlib = D, in_mem = D}.

%% @private
fetch(Key, Memory) ->
  case dict:find(Key, Memory) of
    error -> [];
    {ok, Value} -> Value
  end.

%% @private
store(Key, Value, Memory) ->
  dict:store(Key, Value, Memory).

%% @private
check_abolish(F, Func, Params, State = #state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib}) ->
  case dict:erase(Func, ExLib) of
    ExLib ->  %dict not changed - was not deleted. Search userspace
      do_action(Db, F, {StdLib, ExLib, DbState}, Params, State);
    UExlib -> %dict changed -> was deleted
      {reply, ok, State#state{exlib = UExlib}}
  end.

%% @private
do_action(Db, Fun, Memory, Params, State) ->
  try
    {Res, UState} = Db:Fun(Memory, Params),
    {reply, Res, State#state{state = UState}}
  catch
    throw:E -> {reply, E, State}
  end.

%% @private
check_immutable(Dict, Functor) ->
  case dict:is_key(Functor, Dict) of
    false -> ok;
    true ->
      erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor)) %TODO will crash db process, but not erlog
  end.