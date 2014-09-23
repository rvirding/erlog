%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Сент. 2014 16:09
%%%-------------------------------------------------------------------
-module(erlog_simple_debugger).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/0, configure/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
{
  policy = listing %default policy of debugger is listing.
}).
%% policy can be step - make N commands and

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec configure(pid()) -> ok.
configure(Debugger) -> gen_server:call(Debugger, conf, infinity).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(conf, _From, State) ->
  Policy = process_action(),
  {reply, ok, State#state{policy = Policy}};
handle_call({_, Functor, Vars}, _From, State = #state{policy = {stop, Pred}}) ->  %stopping
  Polisy = case lists:flatten(io_lib:format("~p", [Functor])) of
             Pred ->
               io:fwrite("Erlog debugger stopped execution on command ~s with memory: ~p.~n", [Pred, process_reply(Vars)]),
               process_action();
             Other ->
               io:format("Skip ~s~n", [Other]),
               {stop, Pred}
           end,
  {reply, ok, State#state{policy = Polisy}};
handle_call({_, Functor, Vars}, _From, State = #state{policy = {next, N}}) when N =< 1 -> %counting steps ending
  io:fwrite("Erlog debugger stopped execution on command ~p with memory: ~p.~n", [Functor, process_reply(Vars)]),
  Policy = process_action(),
  {reply, ok, State#state{policy = Policy}};
handle_call({_, Functor, _}, _From, State = #state{policy = {next, N}}) ->  %counting steps
  io:fwrite("Skip ~p~n", [Functor]),
  {reply, ok, State#state{policy = {next, N - 1}}};
handle_call({_Res, Functor, Vars}, _From, State) -> %listing
  io:format("Execute ~p, memory: ~p~n", [Functor, process_reply(Vars)]),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
process_reply(Dict) ->
  case dict:is_empty(Dict) of
    true -> [];
    false -> process_vars(Dict)
  end.

%% @private
process_vars(Dict) ->
  Keys = dict:fetch_keys(Dict),
  lists:foldl(
    fun(Key, Res) ->
      case dict:find(Key, Dict) of
        {ok, {K}} ->
          process_values(Key, K, Dict, Res);
        _ -> Res
      end
    end, [], Keys).

%% @private
process_values(Key, K, Dict, Res) ->
  case dict:find(K, Dict) of
    {ok, V} -> [{Key, V} | Res];
    error -> Res
  end.

%% @private
%% Is called when code execution is stopped. Waits for user action.
process_action() ->
  io:format("Select action~n"),
  Order = io:get_line('| ?- '),
  Listing = lists:prefix("listing", Order),
  Next = lists:prefix("next", Order),
  Stop = lists:prefix("stop", Order),
  if
    Listing -> listing;
    Next -> process_next(Order);
    Stop -> process_stop(Order);
    true ->
      io:format("Wrong action!~n"),
      process_action()
  end.

%% @private
process_next(Next) ->
  N = Next -- "next ",
  {Num, _Rest} = string:to_integer(N),
  {next, Num}.

%% @private
process_stop(Stop) ->
  {stop, Stop -- "stop \n"}.