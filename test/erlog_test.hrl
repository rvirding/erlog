-include("../src/erlog_int.hrl").
%% Copyright (c) 2014 Zachary Kessin
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.



get_quickcheck_properties() ->
    Funs  = ?MODULE:module_info(functions),
    Funs1 = [P || {P, 0} <- Funs],
    Props = lists:filter(fun(Fun) ->
				 FnName = atom_to_list(Fun),
				 "prop_" =:= string:sub_string(FnName, 1,5)
			 end, Funs1),
    Props.

run_quickcheck_properties_test_() ->
    run_quickcheck(get_quickcheck_properties()).

run_quickcheck(Tests) ->
    run_quickcheck(Tests,100).

run_quickcheck(Tests, _Count) ->
    begin
	[begin
	     P1 = ?MODULE:Prop(),
	     P2 = out(P1),
	     ?_assert(eqc:quickcheck(P2))
	 end  || Prop<-Tests]
    end.

%-------------------------------------------------------------------------------

out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).
