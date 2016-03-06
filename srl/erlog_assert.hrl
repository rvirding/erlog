-define(assertProlog(PrologExp, Erlog),
        begin
            ((fun() ->
                      case (erlog:prove(PrologExp,Erlog)) of 
                          {{succeed,_}, E1} ->
                              {ok,E1};
                          {fail, E1} ->
                              erlang:error({assertProlog, [
                                                           [{module, ?MODULE},
                                                            {line, ?LINE},
                                                            {expression, PrologExp},
                                                            {expected, true},
                                                            {erlog_state, E1}
                                                           ]]})
                      end
              end)())
        end).


-define(assertPrologFail(PrologExp, Erlog),
        begin
            ((fun() ->
                      case (erlog:prove(PrologExp,Erlog)) of 
                          {fail, E1} ->
                              {ok,E1};
                          {_, E1} ->
                              erlang:error({assertProlog, [
                                                           [{module, ?MODULE},
                                                            {line, ?LINE},
                                                            {expression, PrologExp},
                                                            {expected, true},
                                                            {erlog_state, E1}
                                                           ]]})
                      end
              end)())
        end).

