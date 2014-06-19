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

-callback add_built_in(State, Functor) -> {ok, NewState} | {error, Reason}.

-callback add_compiled_proc(State, {Functor, M, F}) -> {ok, NewState}  | {error, Reason}.

-callback assertz_clause(State, {Head, Body}) -> {ok, NewState}  | {error, Reason}.

-callback asserta_clause(State, {Head, Body}) -> {ok, NewState}  | {error, Reason}.

-callback retract_clause(State, {F, Ct}) -> {ok, NewState}  | {error, Reason}.

-callback abolish_clauses(State, Func) -> {ok, NewState}  | {error, Reason}.

-callback get_procedure(State, Func) -> {atom, NewState}  | {term(), NewState} | {error, Reason}.

-callback get_procedure_type(State, Func) -> {atom(), NewState}  | {error, Reason}.

-callback get_interp_functors(State) -> {list(), NewState}  | {error, Reason}.