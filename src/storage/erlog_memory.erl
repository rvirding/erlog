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

-include("erlog_int.hrl").

%% API
-export([start_link/1, add_compiled_proc/2, assertz_clause/3, asserta_clause/3,
	retract_clause/3, abolish_clauses/2, get_procedure/2, get_procedure_type/2,
	get_interp_functors/1, assertz_clause/2, asserta_clause/2]).

-export([add_built_in/2]).

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
add_built_in(Database, Element) -> gen_server:call(Database, {add_built_in, Element}).

add_compiled_proc(Database, Proc) -> gen_server:call(Database, {add_compiled_proc, Proc}).

assertz_clause(Database, {':-', Head, Body}) -> assertz_clause(Database, Head, Body);
assertz_clause(Database, Head) -> assertz_clause(Database, Head, true).
assertz_clause(Database, Head, Body) -> gen_server:call(Database, {assertz_clause, {Head, Body}}).

asserta_clause(Database, {':-', H, B}) -> asserta_clause(Database, H, B);
asserta_clause(Database, H) -> asserta_clause(Database, H, true).
asserta_clause(Database, Head, Body) -> gen_server:call(Database, {asserta_clause, {Head, Body}}).

retract_clause(Database, F, Ct) -> gen_server:call(Database, {retract_clause, {F, Ct}}).

abolish_clauses(Database, Func) -> gen_server:call(Database, {abolish_clauses, Func}).

get_procedure(Database, Func) -> gen_server:call(Database, {get_procedure, Func}).

get_procedure_type(Database, Func) -> gen_server:call(Database, {get_procedure_type, Func}).

get_interp_functors(Database) -> gen_server:call(Database, get_interp_functors).

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
	State = Database:new(),
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
handle_call(Fun, _From, State = #state{state = State, database = Database}) ->
	{Res, NewState} = Database:Fun(State),
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
