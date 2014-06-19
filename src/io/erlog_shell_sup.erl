%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. май 2014 20:04
%%%-------------------------------------------------------------------
-module(erlog_shell_sup).
-author("tihon").

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
% for console
start_socket() ->	supervisor:start_child(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
		MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
		[ChildSpec :: supervisor:child_spec()]
	}} |
	ignore |
	{error, Reason :: term()}).
init([]) ->
	{ok, Port} = application:get_env(console_port),
	Opts = [{active, true}, {keepalive, true}, {packet, 0}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opts) of
		{ok, ListenSocket} ->
			io:fwrite("~w:Listening on port ~p~n", [?MODULE, Port]),  %TODO lager
			RestartStrategy = {simple_one_for_one, 10, 60},
			Listener = {erlog_shell, {erlog_shell, start_link, [{tcp, ListenSocket}]},
				temporary, 2000, worker, [erlog_shell]},
			spawn_link(fun start_socket/0),
			{ok, {RestartStrategy, [Listener]}};
		{error, Reason} ->
			io:format("Can't start server on ~p port!~nError: ~p", [Port, Reason]),
			{stop, Reason};
		Other ->
			io:format("Can't start server on ~p port!~nReason: ~p", [Port, Other]),
			{stop, Other}
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================