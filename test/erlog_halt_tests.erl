-module(erlog_halt_tests).
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

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

erlog_halt_test() ->
    {Pid,Ref} = spawn_monitor(fun() ->
				      {ok,Erlog} = erlog:new(),

				      erlog:prove(Erlog, {halt, test}),
				      timer:sleep(300),
				      ok
				end),
    receive
	{'DOWN', Ref, process, Pid, R}  ->
	    ?assertEqual(test, R),
	    ok
    after 20 ->
	    ?debugVal("No Halt"),
	    ?assert(false)
    end.
