-module(erlog_ets_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").

-compile(export_all).



% erlog_ets_all_test() ->
%     {ok, ERLOG}   = erlog_:new(),
%     ok = erlog:load(ERLOG,erlog_ets),
%     TabId = ets:new(test_ets_table, [bag, {keypos,2}, named_table]),
%     ?assertEqual({succeed,[]},erlog:prove(ERLOG, {ets_all, test_ets_table})),
%     ok.

erlog_empty_ets_test() ->
    {ok, ERLOG}		= erlog:new(),
    {ok, ERLOG1}	= erlog:load(ERLOG,erlog_ets),
    TabId		= ets:new(test_ets_table, [bag, {keypos,2}]),
    {fail,#est{}}	= erlog:prove(ERLOG1, {ets_keys, TabId, {'S'}}),
    {fail,#est{}}	= erlog:prove(ERLOG1, {ets_match, TabId,{'S'}}),
    true.

    
     
gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).

prop_ets_keys() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, ERLOG}   = erlog:new(),
		{ok, ERLOG1}  = erlog:load(ERLOG,erlog_ets),
		TabId = ets:new(test_ets_table, [bag, {keypos,2}]),
		ets:insert(TabId, Nodes),
		lists:all(fun({edge,S,_E})->
				  {{succeed, []},#est{}} = erlog:prove(ERLOG1, {ets_keys, TabId, S}),
				  true
			  end, Nodes)
		end).


prop_ets_match_all() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, ERLOG}   = erlog:new(),
		{ok, ERLOG1} = erlog:load(ERLOG,erlog_ets),
		TabId = ets:new(test_ets_table, [bag]),
		ets:insert(TabId, Nodes),

		true = lists:all(fun(Edge = {edge,_,_})->
					 {{succeed, []},#est{}}  = erlog:prove(ERLOG1, {ets_match, TabId, Edge}),
					 true
			  end, Nodes)
		end).

prop_ets_match() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, ERLOG}   = erlog:new(),
		{ok, ERLOG1} = erlog:load(ERLOG,erlog_ets),
		TabId = ets:new(test_ets_table, [bag]),
		ets:insert(TabId, Nodes),

                case  erlog:prove(ERLOG1, {ets_match, TabId, {'X'}}) of
                    {{succeed,[{'X', M}]},#est{}} -> 
			lists:member(M,Nodes);
                    _R           -> 
			false
                end
            end).

