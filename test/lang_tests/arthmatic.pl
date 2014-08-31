% -*- Prolog -*-
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


test(_) :-
    8  is  6+2,
    8  is  4*2,
    8  is  9 - 1,
    8  is  2 ** 3,
    8  is  16 / 2,
    9  is  3 * 3,
    8  is  abs(8),
    8  is  abs(+ 8),
    8  is  abs(-8),
    1  is  mod(7,2).
