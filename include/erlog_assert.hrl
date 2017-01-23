
%% Copyright (c) 2016 Zachary Kessin
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
%% limitation

-define(assertProlog(PrologExp, Erlog),
        begin
            ((fun() ->
                      case (erlog:prove(PrologExp,Erlog)) of 
                          {{succeed,_}, E1} ->
                              {ok,E1};
                          {fail, E1} ->
                              erlang:error({assertProlog, [
                                                           [{module, ?MODULE},
                                                            {line, ?LINE},
                                                            {expression, PrologExp},
                                                            {expected, true},
                                                            {erlog_state, E1}
                                                           ]]})
                      end
              end)())
        end).


-define(assertPrologFail(PrologExp, Erlog),
        begin
            ((fun() ->
                      case (erlog:prove(PrologExp,Erlog)) of 
                          {fail, E1} ->
                              {ok,E1};
                          {_, E1} ->
                              erlang:error({assertProlog, [
                                                           [{module, ?MODULE},
                                                            {line, ?LINE},
                                                            {expression, PrologExp},
                                                            {expected, true},
                                                            {erlog_state, E1}
                                                           ]]})
                      end
              end)())
        end).

