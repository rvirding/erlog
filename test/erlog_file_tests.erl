-module(erlog_file_tests).
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
-include("erlog_test.hrl").
-compile(export_all).


consult_no_file_test() ->
    {ok, ERLOG}    = erlog:new(),
    ?assertMatch({error,enoent}, erlog:consult(ERLOG,   "no_file.pl")),
    ?assertMatch({error,enoent}, erlog:reconsult(ERLOG, "no_file.pl")),
    true.

consult_with_file_test()->
    application:set_env(erlog, consult_path, [".", "../stdlib", "../priv", "../test"]),
    {ok, ERLOG}  = erlog:new(),
    {ok, ERLOG1} =  erlog:consult(ERLOG,    "graph.pl"),
    {ok, ERLOG2} =  erlog:reconsult(ERLOG1, "graph.pl"),
    ?assert(is_record(ERLOG2,est)),
    true.
    
