%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% server for handling telnet (tcp, plaintext) connections.
%%% Such connecitons are used for debug purpose
%%% @end
%%% Created : 26. май 2014 20:05
%%%-------------------------------------------------------------------
-module(erlog_shell).
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
	{noreply, State#state{socket = AcceptSocket, core = erlog:new()}};
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
handle_info({tcp, _, CommandRaw}, State) ->
	try
		process_command(CommandRaw, State)
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
terminate(_Reason, #state{socket = Socket}) -> %TODO destroy core
	gen_tcp:close(Socket),
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
% processes command and send it to prolog
process_command(CommandRaw, State = #state{spike = select, core = Core}) ->
	Reply = erlog_shell_logic:process_command(Core, {select, CommandRaw}),
	process_reply(State, Reply);
process_command(CommandRaw, State = #state{line = Line}) when Line /= [] -> %TODO handle ^C
	process_command(lists:append(Line, CommandRaw), State#state{line = []});  % collect all preceeding dot chunks
process_command(CommandRaw, State = #state{line = Line, socket = Socket}) ->
	case erlog_scan:tokens([], CommandRaw, 1) of
		{done, Result, _Rest} -> run_command(Result, State); % command is finished
		{more, _} ->  % unfinished command. Save chunk and ask for next.
			gen_tcp:send(Socket, <<"| ?- ">>),
			{noreply, State#state{line = lists:append(Line, CommandRaw)}}
	end.
% run full scanned command
run_command(Command, State = #state{core = Logic, socket = Socket}) ->
	case erlog_parse:parse_prolog_term(Command) of
		{ok, halt} ->
			gen_tcp:send(Socket, <<"Ok.\n">>),
			{stop, normal, State};
		PrologCmd ->
			Reply = erlog_shell_logic:process_command(Logic, PrologCmd),
			process_reply(State, Reply)
	end.
% process reply from prolog %TODO find better way to handle erlog_shell_logic:show_bindings selection
process_reply(State = #state{socket = Socket}, {NewCore, Res}) ->
	gen_tcp:send(Socket, Res),
	gen_tcp:send(Socket, <<"\n| ?- ">>),
	{noreply, State#state{core = NewCore, spike = normal}};
process_reply(State = #state{socket = Socket}, {NewCore, Res, select}) ->
	gen_tcp:send(Socket, Res),
	gen_tcp:send(Socket, <<"\n: ">>),
	{noreply, State#state{core = NewCore, spike = select}}.