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

-callback close(Pid :: pid()) -> ok.

-callback next(Pid :: pid()) -> ok.

-callback assertz_clause({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Param :: term()) -> {ok, NewState :: term()}.

-callback asserta_clause({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Param :: term()) -> {ok, NewState :: term()}.

-callback findall({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Functor :: tuple()) -> {Res :: list(), NewState :: term()}.

-callback listing({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Param :: term()) -> {Res :: list(), NewState :: term()}.

-callback retract_clause({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Param :: term()) -> {ok, NewState :: term()}.

-callback abolish_clauses({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Func :: term()) -> {ok, NewState :: term()}.

-callback get_procedure({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Func :: term()) -> {atom, NewState :: term()}  | {term(), NewState :: term()}.

-callback get_procedure_type({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}, Func :: term()) -> {atom(), NewState :: term()}.

-callback get_interp_functors({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: term()}) -> {list(), NewState :: term()}.