%% Copyright (c) 2008-2013 Robert Virding
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

%% File    : erlog_boot.erl
%% Author  : Robert Virding
%% Purpose : Erlog boot module.

%% This little beauty allows you to start Erlang with the Erlog shell
%% running and still has ^G and user_drv enabled. Use it as follows:
%%
%% erl -noshell -noinput -s erlog_boot start
%%
%% NOTE order of commands important, must be -noshell -noinput! Add
%% -pa to find modules if necessary.
%%
%% Thanks to Attila Babo for showing me how to do this.

-module(erlog_boot).

-export([start/0]).

start() -> user_drv:start(['tty_sl -c -e', {erlog_shell, start, []}]).
