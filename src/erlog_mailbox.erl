-module(erlog_mailbox).

-include("erlog_int.hrl").

-compile(export_all).


send(Pid, Msg) ->
    Pid ! Msg,
    {succeed_last,Msg}.

receive_msg(Timeout) ->
    receive
	Msg ->
	    {succeed_last, Msg}
    after Timeout ->
	    fail
    end.

