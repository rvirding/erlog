-module(erlog_lists_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_append_lists() ->
    ?FORALL(
       {A,B},
       {list(int()), list(int())},
       begin
           Term = {append,A,B,{'Z'}},
           {ok, PID} = erlog:start_link(),
           case  erlog:prove(PID,Term) of
               {succeed, [{'Z', Z}]} ->
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
           {ok, PID} = erlog:start_link(),
           case  erlog:prove(PID,Term) of
               {succeed, [{'A', A}, 
                          {'B', B}]} ->
                   L =:= lists:append(A,B);
               fail ->
                   false
           end
       end).


prop_reverse_list() ->
    ?FORALL(L, list(int()),
            begin
                Term =  {reverse,L,{'Y'}},
                {ok, PID} = erlog:start_link(),
                case  erlog:prove(PID,Term) of
                    {succeed, [{'Y', Y}]} ->
                        L =:= lists:reverse(Y);
                    fail ->
                        false
                end
            end).

run_test() ->
    ?assert(proper:quickcheck(prop_append_list(), 
                              [{to_file, user},200])),
    ?assert(proper:quickcheck(prop_append_lists(), 
                              [{to_file, user},200])),
    ?assert(proper:quickcheck(prop_reverse_list(), 
                              [{to_file, user},200])),

    ok.
