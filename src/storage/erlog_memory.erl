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

-behaviour(gen_server).

%% API
-export([start_link/1,
	start_link/2,
	load_library_space/2,
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
	listing/1]).

-export([db_assertz_clause/3,
	db_assertz_clause/4,
	db_asserta_clause/4,
	db_asserta_clause/3,
	db_retract_clause/4,
	db_abolish_clauses/3,
	get_db_procedure/3,
	db_findall/3,
	db_listing/2]).

-export([load_kernel_space/3]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
{
	database :: atom(), % callback module
	state :: term() % callback state
}).

%%%===================================================================
%%% API
%%%===================================================================
%% kernelspace predicate loading
load_kernel_space(Database, Module, Element) -> gen_server:call(Database, {load_kernel_space, {Module, Element}}).

%% libraryspace predicate loading
load_library_space(Database, Proc) -> gen_server:call(Database, {load_library_space, {Proc}}).

%% userspace predicate loading
assertz_clause(Database, {':-', Head, Body}) -> assertz_clause(Database, Head, Body);
assertz_clause(Database, Head) -> assertz_clause(Database, Head, true).
assertz_clause(Database, Head, Body) -> gen_server:call(Database, {assertz_clause, {Head, Body}}).

db_assertz_clause(Database, Collection, {':-', Head, Body}) -> db_assertz_clause(Database, Collection, Head, Body);
db_assertz_clause(Database, Collection, Head) -> db_assertz_clause(Database, Collection, Head, true).
db_assertz_clause(Database, Collection, Head, Body) ->
	gen_server:call(Database, {assertz_clause, {Collection, Head, Body}}).

asserta_clause(Database, {':-', H, B}) -> asserta_clause(Database, H, B);
asserta_clause(Database, H) -> asserta_clause(Database, H, true).
asserta_clause(Database, Head, Body) -> gen_server:call(Database, {asserta_clause, {Head, Body}}).

db_asserta_clause(Database, Collection, {':-', H, B}) -> db_asserta_clause(Database, Collection, H, B);
db_asserta_clause(Database, Collection, H) -> db_asserta_clause(Database, Collection, H, true).
db_asserta_clause(Database, Collection, Head, Body) ->
	gen_server:call(Database, {asserta_clause, {Collection, Head, Body}}).

db_findall(Database, Collection, Fun) -> gen_server:call(Database, {findall, {Collection, Fun}}).
finadll(Database, Fun) -> gen_server:call(Database, {findall, {Fun}}).

retract_clause(Database, F, Ct) -> gen_server:call(Database, {retract_clause, {F, Ct}}).
db_retract_clause(Database, Collection, F, Ct) -> gen_server:call(Database, {retract_clause, {Collection, F, Ct}}).

abolish_clauses(Database, Func) -> gen_server:call(Database, {abolish_clauses, {Func}}).
db_abolish_clauses(Database, Collection, Func) -> gen_server:call(Database, {abolish_clauses, {Collection, Func}}).

get_procedure(Database, Func) -> gen_server:call(Database, {get_procedure, {Func}}).
get_db_procedure(Database, Collection, Func) -> gen_server:call(Database, {get_procedure, {Collection, Func}}).

get_procedure_type(Database, Func) -> gen_server:call(Database, {get_procedure_type, {Func}}).

get_interp_functors(Database) -> gen_server:call(Database, get_interp_functors).

raw_store(Database, Key, Value) -> gen_server:call(Database, {raw_store, {Key, Value}}).

raw_fetch(Database, Key) -> gen_server:call(Database, {raw_fetch, {Key}}).

raw_append(Database, Key, Value) -> gen_server:call(Database, {raw_append, {Key, Value}}).

raw_erase(Database, Key) -> gen_server:call(Database, {raw_erase, {Key}}).

listing(Database) -> gen_server:call(Database, listing).

db_listing(Database, Collection) -> gen_server:call(Database, {listing, Collection}).

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
	{ok, #state{database = Database, state = State}};
init([Database, Params]) when is_atom(Database) ->
	{ok, State} = Database:new(Params),
	{ok, #state{database = Database, state = State}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call({Fun, Params}, _From, State = #state{state = DbState, database = Database}) ->
	{Res, NewState} = Database:Fun(DbState, Params),
	{reply, Res, State#state{state = NewState}};
handle_call(Fun, _From, State = #state{state = DbState, database = Database}) ->
	{Res, NewState} = Database:Fun(DbState),
	{reply, Res, State#state{state = NewState}};
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