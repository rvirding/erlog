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

-callback add_built_in(State :: term(), Functor :: term()) -> {ok, NewState :: term()}.

-callback add_compiled_proc(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback assertz_clause(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback asserta_clause(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback retract_clause(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback abolish_clauses(State :: term(), Func :: term()) -> {ok, NewState :: term()}.

-callback get_procedure(State :: term(), Func :: term()) -> {atom, NewState :: term()}  | {term(), NewState :: term()}.

-callback get_procedure_type(State :: term(), Func :: term()) -> {atom(), NewState :: term()}.

-callback get_interp_functors(State :: term()) -> {list(), NewState :: term()}.