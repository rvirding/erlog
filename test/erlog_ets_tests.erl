-module(erlog_ets_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).



% erlog_ets_all_test() ->
%     {ok, PID}   = erlog:start_link(),
%     ok = erlog:load(PID,erlog_ets),
%     TabId = ets:new(test_ets_table, [bag, {keypos,2}, named_table]),
%     ?assertEqual({succeed,[]},erlog:prove(PID, {ets_all, test_ets_table})),
%     ok.

prop_erlog_empty_ets() ->
    {ok, PID}   = erlog:start_link(),
    ok = erlog:load(PID,erlog_ets),
    TabId = ets:new(test_ets_table, [bag, {keypos,2}]),
    ?assertEqual(fail,erlog:prove(PID, {ets_keys, TabId, {'S'}})),
    ?assertEqual(fail,erlog:prove(PID, {ets_match, TabId,{'S'}})),
    true.

    
     
gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).

prop_ets_keys() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, PID}   = erlog:start_link(),
		ok = erlog:load(PID,erlog_ets),
		TabId = ets:new(test_ets_table, [bag, {keypos,2}]),
		ets:insert(TabId, Nodes),
		lists:all(fun({edge,S,_E})->
				  {succeed, []} =:= erlog:prove(PID, {ets_keys, TabId, S})

			  end, Nodes)
		end).


prop_ets_match_all() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, PID}   = erlog:start_link(),
		ok = erlog:load(PID,erlog_ets),
		TabId = ets:new(test_ets_table, [bag]),
		ets:insert(TabId, Nodes),

		true = lists:all(fun(Edge = {edge,_,_})->
				  {succeed, []} =:= erlog:prove(PID, {ets_match, TabId, Edge})

			  end, Nodes)
		end).

prop_ets_match() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, PID}   = erlog:start_link(),
		ok = erlog:load(PID,erlog_ets),
		TabId = ets:new(test_ets_table, [bag]),
		ets:insert(TabId, Nodes),

                case  erlog:prove(PID, {ets_match, TabId, {'X'}}) of
                    {succeed,[{'X', M}]} -> 
			lists:member(M,Nodes);
                    _R           -> 
			false
                end
            end).



