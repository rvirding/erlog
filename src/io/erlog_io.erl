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

%% File    : erlog_io.erl
%% Author  : Robert Virding
%% Purpose : Some basic i/o functions for Erlog.
%%
%% Structures	- {Functor,arg1, Arg2,...} where Functor is an atom
%% Variables	- {Name} where Name is an atom or integer
%% Lists	- Erlang lists
%% Atomic	- Erlang constants
%%
%% There is no problem with the representation of variables as Prolog
%% functors of arity 0 are atoms. This representation is much easier
%% to test for, and create new variables with than using funny atom
%% names like '$1' (yuch!), and we need LOTS of variables.

-module(erlog_io).

-behaviour(erlog_file_consulter).

-export([format_error/1, format_error/2, lookup/1, load/1]).

-spec lookup(Directory :: string()) -> list().
lookup(Directory) ->
  case file:list_dir(Directory) of
    {ok, List} -> List;
    {error, enoent} -> []
  end.

%% Read a file containing Prolog terms. This has been taken from 'io'
%% but cleaned up using try.
-spec load(File :: string()) -> {ok, [Term :: term()]} | {error, Error :: term()}.
load(File) ->
  case file:open(File, [read]) of
    {ok, Fd} ->
      try
        {ok, read_stream(Fd, 1)}
      catch
        throw:Term -> Term;
        error:Error -> {error, {einval, Error}};
        exit:Exit -> {exit, {einval, Exit}}
      after
        file:close(Fd)
      end;
    Error -> Error
  end.

format_error(Params) -> format_error("Error", Params).
format_error(Type, Params) ->
  B = lists:foldr(
    fun(Param, Acc) when is_list(Param) ->
      [Param | Acc];
      (Param, Acc) ->
        [io_lib:format("~p", [Param]) | Acc]
    end, ["\n"], [Type | Params]),
  S = string:join(B, ": "),
  lists:flatten(S).


%% @private
read_stream(Fd, L0) ->
  case scan_erlog_term(Fd, '', L0) of
    {ok, Toks, L1} ->
      case erlog_parse:term(Toks, L0) of
        {ok, end_of_file} -> [];    %Prolog does this.
        {ok, Term} -> [Term | read_stream(Fd, L1)]; %TODO recurstion is not tail!
        {error, What} -> throw({error, What})
      end;
    {error, Error, _} -> throw({error, Error});
    {eof, _} -> []
  end.

%% @private
scan_erlog_term(Io, Prompt, Line) ->
  io:request(Io, {get_until, Prompt, erlog_scan, tokens, [Line]}).