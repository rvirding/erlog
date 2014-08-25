%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 18:00
%%%-------------------------------------------------------------------

-module(erlog_ets).

-behaviour(erlog_storage).

%% erlog callbacks
-export([new/0, new/1,
	assertz_clause/2,
	asserta_clause/2,
	retract_clause/2,
	abolish_clauses/2,
	get_procedure/2,
	get_procedure_type/2,
	get_interp_functors/1,
	findall/2,
	listing/2]).

new() -> {ok, ets:new(eets, [])}.

new(_) -> {ok, ets:new(eets, [])}.

assertz_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = assertz_clause({StdLib, ExLib, Ets}, {Head, Body0}),
	{Res, Db};
assertz_clause({_, _, Db} = Memory, {Head, Body0}) ->
	clause(Head, Body0, Memory,
		fun(Functor, Tag, Cs, Body) ->
			case check_duplicates(Cs, Head, Body) of
				false -> ok;  %found - do nothing
				_ -> ets:insert(Db, {Functor, clauses, Tag + 1, Cs ++ [{Tag, Head, Body}]}) %not found - insert new
			end
		end),
	{ok, Db}.

asserta_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = asserta_clause({StdLib, ExLib, Ets}, {Head, Body0}),
	{Res, Db};
asserta_clause({_, _, Db} = Memory, {Head, Body0}) ->
	clause(Head, Body0, Memory,
		fun(Functor, Tag, Cs, Body) ->
			case check_duplicates(Cs, Head, Body) of
				false -> ok;  %found - do nothing
				_ -> ets:insert(Db, {Functor, clauses, Tag + 1, [{Tag, Head, Body} | Cs]}) %not found - insert new
			end
		end),
	{ok, Db}.

retract_clause({StdLib, ExLib, Db}, {Collection, Functor, Ct}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = retract_clause({StdLib, ExLib, Ets}, {Functor, Ct}),
	{Res, Db};
retract_clause({StdLib, ExLib, Db}, {Functor, Ct}) ->
	ok = check_immutable(StdLib, Db, Functor),
	ok = check_immutable(ExLib, Db, Functor),
	case ets:lookup(Db, Functor) of
		[{_, clauses, Nt, Cs}] ->
			ets:insert(Db, {Functor, clauses, Nt, lists:keydelete(Ct, 1, Cs)});
		[] -> ok        %Do nothing
	end,
	{ok, Db}.

abolish_clauses({StdLib, ExLib, Db}, {Collection, Functor}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = abolish_clauses({StdLib, ExLib, Ets}, {Functor}),
	{Res, Db};
abolish_clauses({StdLib, ExLib, Db}, {Functor}) ->
	ok = check_immutable(StdLib, Db, Functor),
	case ets:lookup(ExLib, Functor) of  %delete from library-space
		[{_, code, _}] -> ets:delete(ExLib, Functor);
		[] -> %if not found - delete from userspace
			case ets:lookup(Db, Functor) of
				[{_, clauses, _, _}] -> ets:delete(Db, Functor);
				[] -> ok        %Do nothing
			end
	end,
	{ok, Db}.

findall({StdLib, ExLib, Db}, {Collection, Functor}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = findall({StdLib, ExLib, Ets}, {Functor}),
	{Res, Db};
findall({StdLib, ExLib, Db}, {Functor}) ->
	Params = tuple_to_list(Functor),
	Fun = hd(Params),
	Len = length(Params) - 1,
	case ets:lookup(StdLib, Functor) of %search built-in first
		[{Bin, {built_in, _}}] -> {Bin, Db};
		[] ->
			case ets:lookup(ExLib, Functor) of  %search libraryspace then
				[{_, code, Lib}] -> {Lib, Db};
				[] ->
					case ets:lookup(Db, {Fun, Len}) of  %search userspace last
						[{_, clauses, _, Body}] -> {Body, Db};
						[] -> {[], Db}
					end
			end
	end.

get_procedure({StdLib, ExLib, Db}, {Collection, Functor}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = get_procedure({StdLib, ExLib, Ets}, {Functor}),
	{Res, Db};
get_procedure({StdLib, ExLib, Db}, {Functor}) ->
	Res = case ets:lookup(StdLib, Functor) of %search built-in first
		      [{_, {built_in, Module}}] -> {built_in, Module};
		      [] ->
			      case ets:lookup(ExLib, Functor) of  %search libraryspace then
				      [{_, code, C}] -> {code, C};
				      [] ->
					      case ets:lookup(Db, Functor) of  %search userspace last
						      [{_, clauses, _, Cs}] -> {clauses, Cs};
						      [] -> undefined
					      end
			      end
	      end,
	{Res, Db}.

get_procedure_type({StdLib, ExLib, Db}, {Functor}) ->
	Res = case ets:lookup(StdLib, Functor) of %search built-in first
		      [{_, {built_in, _}}] -> built_in;
		      [] ->
			      case ets:lookup(ExLib, Functor) of  %search libraryspace then
				      [{_, code, _}] -> compiled;
				      [] ->
					      case ets:lookup(Db, Functor) of  %search userspace last
						      [{_, clauses, _, _}] -> interpreted;
						      [] -> undefined
					      end
			      end
	      end,
	{Res, Db}.

get_interp_functors({_, ExLib, Db}) ->
	Library = ets:foldl(fun({Func, code, _}, Fs) -> [Func | Fs];
		(_, Fs) -> Fs
	end, [], ExLib),

	Res = ets:foldl(fun({Func, clauses, _, _}, Fs) -> [Func | Fs];
		(_, Fs) -> Fs
	end, Library, Db),
	{Res, Db}.

listing({StdLib, ExLib, Db}, {Collection, Params}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = listing({StdLib, ExLib, Ets}, {Params}),
	{Res, Db};
listing({_, _, Db}, {[Functor, Arity]}) ->
	{ets:foldl(
		fun({{F, A} = Res, clauses, _, _}, Acc) when F == Functor andalso A == Arity ->
			[Res | Acc];
			(_, Acc) -> Acc
		end, [], Db), Db};
listing({_, _, Db}, {[Functor]}) ->
	{ets:foldl(
		fun({{F, Arity}, clauses, _, _}, Acc) when F == Functor ->
			[{Functor, Arity} | Acc];
			(_, Acc) -> Acc
		end, [], Db), Db};
listing({_, _, Db}, {[]}) ->
	{ets:foldl(
		fun({Fun, clauses, _, _}, Acc) -> [Fun | Acc];
			(_, Acc) -> Acc
		end, [], Db), Db}.

%% @private
clause(Head, Body0, {StdLib, ExLib, Db}, ClauseFun) ->
	{Functor, Body} = case catch {ok, ec_support:functor(Head), ec_body:well_form_body(Body0, false, sture)} of
		                  {erlog_error, E} -> erlog_errors:erlog_error(E, Db);
		                  {ok, F, B} -> {F, B}
	                  end,
	ok = check_immutable(StdLib, Db, Functor),  %check built-in functions (read only) for clause
	ok = check_immutable(ExLib, Db, Functor),   %check library functions (read only) for clauses
	case ets:lookup(Db, Functor) of
		[{_, clauses, Tag, Cs}] -> ClauseFun(Functor, Tag, Cs, Body);
		[] -> ets:insert(Db, {Functor, clauses, 1, [{0, Head, Body}]})
	end.

%% @private
-spec check_duplicates(list(), tuple(), tuple()) -> boolean().
check_duplicates(Cs, Head, Body) ->
	lists:foldl(
		fun({_, H, B}, _) when H == Head andalso B == Body -> false;  %find same fact
			(_, Acc) -> Acc
		end, true, Cs).

check_immutable(Ets, Db, Functor) ->
	case ets:lookup(Ets, Functor) of
		[] -> ok;
		_ -> erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db)
	end.