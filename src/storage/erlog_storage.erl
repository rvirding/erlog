%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. июн 2014 23:07
%%%-------------------------------------------------------------------
-module(erlog_storage).
-author("tihon").

-callback add_built_in(Functor) -> ok | {error, Reason}.

-callback add_compiled_proc(Functor, M, F) -> ok | {error, Reason}.

-callback assertz_clause(Head, Body) -> ok | {error, Reason}.

-callback asserta_clause(Head, Body) -> ok | {error, Reason}.

-callback retract_clause(F, Ct) -> ok | {error, Reason}.

-callback abolish_clauses(Func) -> ok | {error, Reason}.

-callback get_procedure(Func) -> ok | {error, Reason}.

-callback get_procedure_type(Func) -> ok | {error, Reason}.

-callback get_interp_functors() -> ok | {error, Reason}.

