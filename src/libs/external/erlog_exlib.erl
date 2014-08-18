%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc erlog external library interface
%%%
%%% @end
%%% Created : 15. Авг. 2014 14:28
%%%-------------------------------------------------------------------
-module(erlog_exlib).
-author("tihon").

%% load database to library space
-callback load(Db :: pid() | atom()) -> ok.