%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Дек. 2014 21:12
%%%-------------------------------------------------------------------
-module(erlog_math).
-author("tihon").

-behaviour(erlog_stdlib).

-include("erlog_math.hrl").
-include("erlog_core.hrl").

%% API
-export([load/1, prove_goal/1]).

load(DbState) ->
  lists:foldl(fun(Proc, UDBState) -> erlog_memory:load_kernel_space(UDBState, ?MODULE, Proc) end, DbState, ?ERLOG_MATH).

prove_goal(Params = #param{goal = {round, _, _, _} = G, next_goal = Next, bindings = Bs0}) ->
  {round, Number, Accuracy, Result} = erlog_ec_support:dderef(G, Bs0),
  Rounded = round_float(Number, Accuracy),
  case erlog_ec_support:try_add(Rounded, Result, Bs0) of
    error -> erlog_errors:fail(Params);
    Bs -> erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
  end.


%% @private
round_float(N, _) when is_integer(N) -> N;
round_float(N, 0) -> round(N);
round_float(F, Accuracy) -> P = math:pow(10, Accuracy), round(F * P) / P.