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

%% ------- Prolog -------
%% add value right
-callback assertz_clause({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Param :: term()) -> {ok, NewState :: any()}.

%% add value left
-callback asserta_clause({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Param :: term()) -> {ok, NewState :: any()}.

%% find all values
-callback findall({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Functor :: tuple()) -> {Res :: list(), NewState :: any()}.

%% get all values in memory by search criteria
-callback listing({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Param :: term()) -> {Res :: list(), NewState :: any()}.

%% remove selected functor
-callback retract_clause({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Param :: term()) -> {ok, NewState :: any()}.

%% remove all matching functors
-callback abolish_clauses({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Func :: term()) -> {ok, NewState :: any()}.

%% ------- System -------
-callback new(Params :: list()) -> {ok, State :: any()}.

%% close cursor
-callback close(State :: any(), Pid :: pid()) -> {ok, NewState :: any()}.

%% get next result by cursor
-callback next(State :: any(), Pid :: any()) -> {[] | any(), NewState :: any()}.

-callback get_procedure({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Func :: term()) -> {atom, NewState :: any()}  | {term(), NewState :: any()}.

-callback get_procedure_type({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}, Func :: term()) -> {atom(), NewState :: any()}.

-callback get_interp_functors({Stdlib :: ets:tid(), ExLib :: ets:tid(), State :: any()}) -> {list(), NewState :: any()}.