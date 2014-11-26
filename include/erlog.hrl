%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Нояб. 2014 18:32
%%%-------------------------------------------------------------------
-author("tihon").

%% Database state
-record(db_state,
{
  stdlib :: dict,  %kernel-space memory
  exlib :: dict, %library-space memory
  database :: atom(), % callback module for user-space memory
  in_mem :: dict, %integrated memory for findall operations
  state :: term() % callback state
}).

%% Core state.
-record(state,
{
  db_state :: #db_state{}, %database state
  f_consulter :: atom(), %file consulter
  debugger :: fun(), %debugger function
  e_man :: pid(), %event manager, used for debuging and other output (not for return)
  state = normal :: normal | list(), %state for solution selecting.
  libs_dir :: string()  %path for directory, where prolog libs are stored
}).