-module(lang_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


swipl_comparison_test() ->
    PLFiles     = get_files(),
    [begin
	 Cmd         = iolist_to_binary(
			 io_lib:format("swipl -q -f ../test/lang_tests/~s -g \"test('~s'),!;halt(1)\" -t halt",[File,File])),

	 ?assertCmd(binary_to_list(Cmd)),
	 true
     end || File <-PLFiles],
    true.
    
	 
prop_lang_test() ->
    PLFiles     = get_files(),
    {ok,PL} = erlog:new(),
    [begin
	 {ok,PL1} = erlog:consult(PL,"../test/lang_tests/"++ File),
	 case  erlog:prove(PL1, {test,File}) of
	     {{succeed, _},_} ->
		 true;
	     {fail, _} ->
		 ?debugFmt("File ~p test/1 fails ~n",[File]),
		 ?assert(false)
	 end
     end || File <- PLFiles],
    true.







get_files() ->
    {ok, Files} = file:list_dir("../test/lang_tests/"),
    lists:filter(fun(File) ->
			 filename:extension(File) =:= ".pl"
                 end, Files).
