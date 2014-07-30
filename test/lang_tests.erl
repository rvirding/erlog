-module(lang_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


prop_lang_test() ->
    {ok, Files} = file:list_dir("../test/lang_tests/"),
    PLFiles     = lists:filter(fun(File) ->
				       filename:extension(File) =:= ".pl"
			       end, Files),
    PL = erlog:new(),
    [begin
	 {ok,PL1} = PL({consult,"../test/lang_tests/"++ File}),
	 case  PL1({prove, {test,File}}) of
	     {{succeed, _},_} ->
		 true;
	     {fail, _} ->
		 ?debugFmt("File ~p test/1 fails ~n",[File]),
		 ?assert(false)
	 end
     end || File <- PLFiles],
    true.





