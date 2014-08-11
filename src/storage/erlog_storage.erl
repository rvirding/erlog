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

-callback new() -> {ok, State :: term()}.

-callback new(Params :: list()) -> {ok, State :: term()}.

-callback add_built_in(State :: term(), Functor :: term()) -> {ok, NewState :: term()}.

-callback add_compiled_proc(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback assertz_clause(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback asserta_clause(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback findall(State :: term(), Functor :: tuple()) -> {Res :: list(), NewState :: term()}.

-callback listing(State :: term()) -> {Res :: list(), NewState :: term()}.

-callback retract_clause(State :: term(), Param :: term()) -> {ok, NewState :: term()}.

-callback abolish_clauses(State :: term(), Func :: term()) -> {ok, NewState :: term()}.

-callback get_procedure(State :: term(), Func :: term()) -> {atom, NewState :: term()}  | {term(), NewState :: term()}.

-callback get_procedure_type(State :: term(), Func :: term()) -> {atom(), NewState :: term()}.

-callback get_interp_functors(State :: term()) -> {list(), NewState :: term()}.

-callback raw_store(State :: term(), Param :: tuple()) -> {ok, NewState :: term()}.

-callback raw_fetch(State :: term(), Param :: tuple()) -> {Value :: any(), NewState :: term()}.

-callback raw_append(State :: term(), Param :: tuple()) -> {ok, NewState :: term()}.

-callback raw_erase(State :: term(), Param :: tuple()) -> {ok, NewState :: term()}.