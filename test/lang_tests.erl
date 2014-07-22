-module(lang_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


prop_lang_test_() ->
    begin
	{ok, Files} = file:list_dir("../test/lang_tests/"),
	PLFiles     = lists:filter(fun(File) ->
					   filename:extension(File) =:= ".pl"
				   end, Files),
	PL = erlog:new(),
	[begin
	     {ok,PL1} = PL({consult,"../test/lang_tests/"++ File}),
	     ?_assertMatch({{succeed, _},_}, PL1({prove, {test,File}}))
	 end || File <- PLFiles]
    end.



