%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Июль 2014 20:07
%%%-------------------------------------------------------------------
-module(erlog_curr_sync).
-author("tihon").

-include("erlog_currency.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, get_course_by_curr/1]).

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
	course :: dict
}).

%%%===================================================================
%%% API
%%%===================================================================
get_course_by_curr(Currency) -> gen_server:call(?MODULE, {get, Currency}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
	gen_server:cast(self(), check),
	timer:send_interval(?CHECK_PERIOD, self(), check),
	{ok, #state{course = dict:new()}}.

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
handle_call({get, Currency}, _From, State = #state{course = Dict}) ->
	Course = dict:find(Currency, Dict),
	{reply, Course, State};
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
handle_cast(check, State = #state{course = Dict}) ->
	UpdCourse = check_course(Dict),
	{noreply, State#state{course = UpdCourse}};
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
handle_info(check, State = #state{course = CourseList}) ->
	UpdCourse = check_course(CourseList),
	{noreply, State#state{course = UpdCourse}};
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
check_course(Current) ->
	CourseJson = get_course(),
	Parsed = jsx:decode(CourseJson),
	parse_course(Parsed, Current).

get_course() ->
%% 	{ok, {{_, 200, _}, _, Body}} = httpc:request(get, {?COURSE_URL, []}, [], []), %TODO!
	Body = <<"[{\"ccy\":\"RUR\",\"base_ccy\":\"UAH\",\"buy\":\"0.32500\",\"sale\":\"0.36000\"},
	{\"ccy\":\"EUR\",\"base_ccy\":\"UAH\",\"buy\":\"15.60000\",\"sale\":\"16.60000\"},
	{\"ccy\":\"USD\",\"base_ccy\":\"UAH\",\"buy\":\"11.65000\",\"sale\":\"11.95000\"}]">>,
	Body.

-spec update_currency(#currency{}, dict) -> dict.
update_currency(Currency = #currency{base_name = BName, name = Name}, Dict) ->
	Key = lists:concat(lists:sort([Name, BName])),
	dict:store(Key, Currency, Dict).

-spec parse_course(list(), dict) -> dict.
parse_course(New, Current) ->
	lists:foldl(
		fun(Proplist, Acc) ->
			try update_currency(parse_currency(Proplist), Acc)
			catch
				_:_ -> Acc
			end
		end, Current, New).

-spec parse_currency(proplists:proplist()) -> #currency{}.
parse_currency(Currency) ->
	Name = parse_value(<<"ccy">>, Currency),
	BaseName = parse_value(<<"base_ccy">>, Currency),
	Buy = parse_value(<<"buy">>, Currency),
	Sell = parse_value(<<"sale">>, Currency),
	#currency{name = Name, base_name = BaseName, buy_course = list_to_float(Buy), sell_course = list_to_float(Sell)}.

-spec parse_value(binary(), proplists:proplist()) -> list().
parse_value(Key, List) ->
	binary_to_list(proplists:get_value(Key, List)).