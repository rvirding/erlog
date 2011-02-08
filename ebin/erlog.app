%% -*- erlang -*-

{application, erlog,
 [{description, "Erlog , Prolog in Erlang"},
  {vsn, "0.6"},
  {modules, [erlog,
	     erlog_boot,
	     erlog_demo,
	     erlog_ets,
	     erlog_int,
             erlog_io,
             erlog_parse,
             erlog_scan,
             erlog_shell,
	     user_pl.erl]},
  {registered, []},
  {applications, [kernel,stdlib]}
 ]}.
