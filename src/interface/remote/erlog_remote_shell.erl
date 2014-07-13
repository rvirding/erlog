%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% server for handling telnet (tcp, plaintext) connections.
%%% Such connecitons are used for debug purpose
%%% @end
%%% Created : 26. май 2014 20:05
%%%-------------------------------------------------------------------
-module(erlog_remote_shell).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/1]).

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
	socket, % client's socket
	core, % erlog function
	line = [],  % current line (not separated with dot).
	spike = normal % this is just a temporary spike, to handle erlog_shell_logic:show_bindings selection
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

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
init({tcp, Socket}) ->
	gen_server:cast(self(), accept),
	{ok, #state{socket = Socket}}.

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
handle_cast(accept, State = #state{socket = ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	erlog_shell_sup:start_socket(),
	Version = list_to_binary(erlang:system_info(version)),
	gen_tcp:send(AcceptSocket, [<<<<"Erlog Shell V">>/binary, Version/binary, <<" (abort with ^G)\n| ?- ">>/binary>>]),
	{ok, Pid} = erlog:start_link([{database, erlog_ets}, {event_h, {erlog_remote_eh, AcceptSocket}}]),
	{noreply, State#state{socket = AcceptSocket, core = Pid}};
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
handle_info({tcp, _, CommandRaw}, State = #state{spike = Spike, line = Line, core = Core, socket = Socket}) ->
	try call_prolog(Spike, Core, Line, CommandRaw) of
		{ok, halt} ->
			gen_tcp:send(Socket, <<"Ok.\n">>),
			{stop, normal, State};
		{ok, more} ->
			gen_tcp:send(Socket, <<"| ?- ">>),
			{noreply, State#state{line = lists:append(Line, CommandRaw)}};
		Reply -> process_reply(State, Reply)
	catch
		_:Msg ->
			gen_tcp:send(State#state.socket, io_lib:format("Error occurred: ~p~n| ? -", [Msg])),
			erlang:display(erlang:get_stacktrace()),
			{noreply, State#state{line = []}}
	end;
handle_info({tcp_error, _}, State) ->
	{stop, normal, State};
handle_info({tcp_closed, _}, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	io:format("~p Unexpected: ~p~n", [?MODULE, _Info]),
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
terminate(_Reason, #state{socket = Socket, core = Core}) ->
	gen_tcp:close(Socket),
	gen_server:cast(Core, halt),
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
call_prolog(select, Core, Line, Term) -> erlog:select(Core, lists:append(Line, Term));
call_prolog(_, Core, Line, Term) -> erlog:execute(Core, lists:append(Line, Term)).

% process reply from prolog
process_reply(State = #state{socket = Socket}, {Res, select}) ->
	io:format("Reply = ~p~n", [Res]),
	gen_tcp:send(Socket, io_lib:format("~p", [Res])),
	gen_tcp:send(Socket, <<"\n: ">>),
	{noreply, State#state{spike = select, line = []}};
process_reply(State = #state{socket = Socket}, Res) ->
	io:format("Reply = ~p~n", [Res]),
	gen_tcp:send(Socket, io_lib:format("~p", [Res])),
	gen_tcp:send(Socket, <<"\n| ?- ">>),
	{noreply, State#state{spike = normal, line = []}}.