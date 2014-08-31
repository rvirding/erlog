-module(lang_tests).
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


swipl_comparison_test_() ->
    PLFiles     = get_files(),
    [begin
	 Cmd         = iolist_to_binary(
			 io_lib:format("swipl -q -f ../test/lang_tests/~s -g \"test('~s'),!;halt(1)\" -t halt",[File,File])),

	 ?_assertCmd(binary_to_list(Cmd))
     end || File <-PLFiles].


    
	 
prop_lang_test_() ->
    PLFiles     = get_files(),
    {ok,PL} = erlog:new(),
    [begin
	 {ok,PL1} = erlog:consult(PL,"../test/lang_tests/"++ File),
	 ?_assertMatch({{succeed, _},_},  erlog:prove(PL1, {test,File}))
     end || File <- PLFiles].



get_files() ->
    {ok, Files} = file:list_dir("../test/lang_tests/"),
    lists:filter(fun(File) ->
			 filename:extension(File) =:= ".pl"
                 end, Files).
