%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc   api for executing prolog code
%%%
%%% @end
%%% Created : 10. июн 2014 18:18
%%%-------------------------------------------------------------------
-module(erlog).
-author("tihon").

%% API
-export([execute/3]).

-spec execute(Command :: string(), Mode :: atom(), Core :: pid) -> any().
execute(Command, select, Core) -> % selection of solution
	process_command(Core, {select, Command});
execute(Command, normal, Core) ->
	case erlog_scan:tokens([], Command, 1) of  % processing command in normal mode
		{done, Result, _Rest} -> run_command(Result, Core); % command is finished, run.
		{more, _} -> {ok, more} % unfinished command. Ask for ending.
	end.

%% @private
%% run full scanned command
run_command(Command, Core) ->
	case erlog_parse:parse_prolog_term(Command) of
		{ok, halt} -> {ok, halt};
		PrologCmd -> process_command(Core, PrologCmd)
	end.

%% @private
%% Gets prolog command and executes it.
-spec process_command(Core :: pid(), tuple()) -> any().
process_command(Core, {ok, Command}) when is_list(Command) ->
	{ok, Db0} = gen_server:call(Core, get_db),
	case erlog_logic:reconsult_files(Command, Db0) of
		{ok, Db1} ->
			{ok, _Db} = gen_server:call(Core, {set_db, Db1}), %TODO if db is connection - disconnect it here?
			<<"Yes">>;
		{error, {L, Pm, Pe}} ->
			erlog_io:format_error([L, Pm:format_error(Pe)]);
		{Error, Message} when Error == error; Error == erlog_error ->
			erlog_io:format_error([Message])
	end;
process_command(Core, {ok, Command}) -> erlog_logic:shell_prove_result(gen_server:call(Core, {prove, Command}));
process_command(_Core, {error, {_, Em, E}}) -> erlog_io:format_error([Em:format_error(E)]);
process_command(Core, {select, Value}) -> erlog_logic:select_bindings(Value, Core).