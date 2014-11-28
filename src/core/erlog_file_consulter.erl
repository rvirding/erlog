%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Окт. 2014 0:22
%%%-------------------------------------------------------------------
-module(erlog_file_consulter).
-author("tihon").

%% get list of files in directory
-callback lookup(Directory :: string()) -> Files :: list().

%% consult selected file
-callback load(FileLoc :: string()) -> {ok, [Term :: term()]} | {error, Error :: term()}.
