%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Авг. 2014 21:57
%%%-------------------------------------------------------------------
-module(erlog_uuid).
-author("tihon").

%TODO remove me from erlog
-include("erlog_uuid.hrl").
-include("erlog_core.hrl").

%% API
-export([load/1, id_1/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:load_library_space(Db, Proc) end, ?ERLOG_UID).

id_1({id, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Bs = ec_support:add_binding(Res, binary_to_list(uuid:generate()), Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).