-module(erlog_lists_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").

prop_append_lists() ->
    ?FORALL(
       {A,B},
       {list(int()), list(int())},
       begin
           Term      = {append,A,B,{'Z'}},
           {ok,E}         = erlog:new(),
           case  erlog:prove(E,Term) of
               {{succeed, [{'Z', Z}]},E1} when is_record(E1, est) ->
                   Z =:= lists:append(A,B);
               fail ->
                   false
           end
       end).


prop_append_list() ->
    ?FORALL(
       L,
       list(int()),
       begin
           Term = {append,{'A'},{'B'},L},
           {ok,E}    = erlog:new(),
           case  erlog:prove(E,Term) of
               {{succeed, [{'A', A}, 
                          {'B', B}]},E1} when is_record(E1,est) ->
                   L =:= lists:append(A,B);
               fail ->
                   false
           end
       end).


prop_reverse_list() ->
    ?FORALL(L, list(int()),
            begin
                Term      =  {reverse,L,{'Y'}},
                {ok,E }        = erlog:new(),
                case  erlog:prove(E,Term) of
                    {{succeed, [{'Y', Y}]},_E1} ->
                        L =:= lists:reverse(Y);
                    fail ->
                        false
                end
            end).



prop_reverse_list_valid() ->
    ?FORALL(L, list(int()),
            begin
                Term =  {reverse,L,lists:reverse(L)},
                {ok, E} = erlog:new(),
                case  erlog:prove(E,Term) of
                    {{succeed, _},_} ->
                        true;
                    {fail,_} ->
                        false
                end
            end).


prop_reverse_list_invalid() ->
    ?FORALL(L, non_empty(list(int())),
            begin
                Term =  {reverse, [1|L], lists:reverse(L)},
                {ok, ERLOG} = erlog:new(),
                case  erlog:prove(ERLOG,Term) of
                    {{succeed, _},_} ->
                        false;
                    {fail, _} ->
                        true
                end
            end).


prop_last_list() ->
    ?FORALL(L, 
            non_empty(list(int())),
            begin
                Term =  {last, lists:last(L),L},
                {ok, ERLOG} = erlog:new(),
                case  erlog:prove(ERLOG,Term) of
                    {{succeed, _},_} ->
                        false;
                    {fail, _} ->
                        true
                end
            end).

prop_member_list() ->
    ?FORALL({M,L,C},
            {int(), list(int()), oneof([member, memberchk])},
            begin
                Term =  {C, M, L},
                {ok, ERLOG} = erlog:new(),
                case  erlog:prove(ERLOG,Term) of
                    {{succeed, _},_} ->
                        lists:member(M,L);
                    {fail, _} ->
                        not(lists:member(M,L))

                end

            end).

prop_sort_list1() ->
    ?FORALL({L},
            { list(int())},
            begin
                Term =  {sort, L, {'Sort'}},
                {ok, ERLOG} = erlog:new(),
                case  erlog:prove(ERLOG,Term) of
                    {{succeed, [{'Sort', Sort}]},_} ->
			lists:usort(L) =:= Sort;
                    {fail, _} ->
			false
                end
            end).

prop_sort_list2() ->
    ?FORALL({L},
            { list(int())},
            begin
                Term =  {sort, L, lists:usort(L)},
                {ok, ERLOG} = erlog:new(),
                case  erlog:prove(ERLOG,Term) of
                    {{succeed, _},_} ->
			true;
                    {fail, _} ->
			false
                end
            end).

% prop_split_with_append() ->
%     ?FORALL(List, 
% 	    non_empty(list(int())),
% 	    ?FORALL(Pivot,choose(1, length(List)),
% 		    begin
% 			E0			= erlog:new(),
% 			{ok,E1}			= E0({consult, "../priv/split.pl"}),
% 			?debugVal(E1({prove, {clause, split, {'x'}}})),
% 			?debugVal({prove, {split, {'Head'}, {'Tail'}, Pivot, List}}),

% 			{{succeed, A }, _E2}	= E1({prove, {split, {'Head'}, {'Tail'}, Pivot, List}}),
% 			?debugVal(A),
% 			Head			= proplists:get_value('Head',A ),
% 			Tail			= proplists:get_value('Tail',A ),
% 			?assertEqual(Pivot, length(Head)),
% 			?assertEqual(List, list:append(Head, Tail)),
% 			true
% 			end)).
