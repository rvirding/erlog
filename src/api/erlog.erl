%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. июн 2014 18:18
%%%-------------------------------------------------------------------
-module(erlog).
-author("tihon").

%% API
-export([process_command/3]).

-spec process_command(CommandRaw :: string(), Spike :: atom(), Core :: pid()) -> any().
process_command(CommandRaw, select, Core) ->  % selecting variants of solutions
	erlog_logic:process_command(Core, {select, CommandRaw});
process_command(CommandRaw, normal, Core) ->
	case erlog_scan:tokens([], CommandRaw, 1) of  % processing command in normal mode
		{done, Result, _Rest} -> run_command(Result, Core); % command is finished
		{more, _} ->  % unfinished command. Save chunk and ask for next.
			{ok, more}
	end.


%% run full scanned command
%% @private
run_command(Command, Logic) ->
	case erlog_parse:parse_prolog_term(Command) of
		{ok, halt} ->
			{ok, halt};
		PrologCmd ->
			erlog_logic:process_command(Logic, PrologCmd)
	end.