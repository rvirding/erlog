%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Янв. 2015 20:06
%%%-------------------------------------------------------------------
-author("tihon").

-define(ERLOG_PARALLEL,
  [
    {spawn, 2}, %spawn goal processing in a new thread
    {join, 2},  %wait for thread (or multiple threads)
    {check, 2}  %get result of thread by pid
  ]).