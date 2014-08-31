-module(lang_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


swipl_comparison_test_() ->
    PLFiles     = get_files(),
    [begin
	 Cmd         = iolist_to_binary(
			 io_lib:format("swipl -q -f ../test/lang_tests/~s -g \"test('~s'),!;halt(1)\" -t halt",[File,File])),

	 ?_assertCmd(binary_to_list(Cmd))
     end || File <-PLFiles].


    
	 
prop_lang_test_() ->
    PLFiles     = get_files(),
    {ok,PL} = erlog:new(),
    [begin
	 {ok,PL1} = erlog:consult(PL,"../test/lang_tests/"++ File),
	 ?_assertMatch({{succeed, _},_},  erlog:prove(PL1, {test,File}))
     end || File <- PLFiles].



get_files() ->
    {ok, Files} = file:list_dir("../test/lang_tests/"),
    lists:filter(fun(File) ->
			 filename:extension(File) =:= ".pl"
                 end, Files).
