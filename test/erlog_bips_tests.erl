-module(erlog_bips_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_equals() ->
    ?FORALL(I, int(),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, [{'X',I}]} =:= erlog:prove(PID, {'=', I, {'X'}})
            end).
prop_not_equals() ->
    ?FORALL(I, int(),
            begin
                {ok, PID}    = erlog:start_link(),
                fail =:= erlog:prove(PID, {'=', I, I + 1})
            end).

prop_float()->
    ?FORALL(I,real(),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {float, I}),
                true
            end).

prop_integer()->
    ?FORALL(I,int(),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {integer, I}),
                true
            end).
prop_number()->
    ?FORALL(I,oneof([int(),real()]),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {number, I}),
                true
            end).

any() ->
    oneof([int(),
           real(),
           char(),
           binary(),
           bitstring(),
           bool()
           ]).

prop_compound() ->
    ?FORALL(V,
            oneof([list(any()),
                   {any()},
                   {any(), any()},
                   {any(), any(), any()},
                   {any(), any(), any(), any()}
                  ]),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {compound, V}),
                true
            end).
       

out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
             fun prop_integer/0,
             fun prop_number/0,
             fun prop_float/0,
             fun prop_equals/0,
             fun prop_not_equals/0

%             fun prop_compound/0
             ],    
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(500,P)))
     end
     || Prop <- Props].
