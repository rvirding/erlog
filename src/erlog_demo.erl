%%% File    : erlog_demo.erl
%%% Author  : Robert Virding
%%% Purpose : Demo functions for Erlang interface of Erlog system.

%%% (C)Robert Virding. This stuff is mine, distributed without
%%% warranty "as is" and may not be used commercially without written
%%% permission.

-module(erlog_demo).

-export([efunc/1,ets_keys/1,get_list/1]).

%% efunc(Fcall) -> {succeed_last,Val}.
%% ets_keys(Table) -> {succeed,Val,Cont} | {succeed_last,Val} | fail.
%% get_list(ListGenerator) -> {succeed,Val,Cont} | {succeed_last,Val} | fail.
%% Test/demo functions for ecall predicate. Examples of different ways
%% of generating solutions.

efunc(Fcall) ->
    %% Call an erlang function and return the value.
    %% This is what the operators will generate.
    Val = case Fcall of
	      {':',M,F} when is_atom(M), is_atom(F) -> M:F();
	      {':',M,{F,A}} when is_atom(M), is_atom(F) -> M:F(A);
	      {':',M,T} when is_atom(M), is_tuple(T), size(T) >= 2,
			     is_atom(element(1, T)) ->
		  apply(M,element(1, T),tl(tuple_to_list(T)))
	  end,
    {succeed_last,Val}.				%Optimisation

ets_keys(Tab) ->
    %% Ets table keys back-trackable.
    %% Solution with no look-ahead, get keys when requested.
    %% This fun returns next key and itself for continuation.
    F = fun (F1, Tab1, Last1) ->
		case ets:next(Tab1, Last1) of
		    '$end_of_table' -> fail;	%No more elements
		    Key1 -> {succeed,Key1, fun () -> F1(F1, Tab1, Key1) end}
		end
	end,
    case ets:first(Tab) of
	'$end_of_table' -> fail;	%No elements
	Key -> {succeed,Key, fun () -> F(F, Tab, Key) end}
    end.

get_list(ListGen) ->
    %% List as back-trackable generator.
    %% This is what the operators will generate.
    Vals = case ListGen of
	       {':',M,F} when is_atom(M), is_atom(F) -> M:F();
	       {':',M,{F,A}} when is_atom(M), is_atom(F) ->
		   M:F(A);
	       {':',M,T} when is_atom(M), is_tuple(T), size(T) >= 2,
				  is_atom(element(1, T)) ->
		   apply(M,element(1, T),tl(tuple_to_list(T)))
	   end,
    %% This fun will return head and itself for continuation.
    Fun = fun (F1, Es0) ->
		  case Es0 of
		      [E] -> {succeed_last,E};	%Optimisation for last one
		      [E|Es] -> {succeed,E,fun () -> F1(F1, Es) end};
		      [] -> fail		%No more elements
		  end
	  end,
    Fun(Fun, Vals).				%Call with list of values
