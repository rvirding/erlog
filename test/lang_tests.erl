-module(lang_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


prop_lang() ->
    {ok, Files} = file:list_dir("test/lang_tests/"),
    PLFiles     = lists:filter(fun(File) ->
				       filename:extension(File) =:= ".pl"
			       end, Files),
    PL = erlog:new(),
    lists:all(fun(File) ->
			  {ok,PL1} = PL({consult,"test/lang_tests/"++ File}),
			  ?assertMatch({{succeed, _},_}, PL1({prove, {test,File}})),
			  true
			  end, PLFiles).



