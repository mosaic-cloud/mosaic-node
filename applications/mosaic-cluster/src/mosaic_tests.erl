
-module (mosaic_tests).


-export ([test_module/1]).


test_module (Module)
		when is_atom (Module) ->
	OldTrapExit = erlang:process_flag (trap_exit, true),
	Tests = lists:flatten (
			lists:map (
				fun
					({test, [{Function, Arguments}]})
							when is_atom (Function), is_list (Arguments) ->
						[{Module, Function, Arguments, 10000}];
					({Name, _Value})
							when (Name =/= test) ->
						[]
				end,
				erlang:apply (Module, module_info, [attributes]))),
	Outcome = execute_tests (Tests),
	true = erlang:process_flag (trap_exit, OldTrapExit),
	ok = case Outcome of
		ok ->
			ok;
		{failed, _Test, Reason} ->
			ok = error_logger:error_report ([lists:flatten (io_lib:format ("Test failed!~n~p", [Reason]))]),
			ok
	end,
	ok.

execute_tests ([]) ->
	ok;
	
execute_tests ([Test = {Module, Function, Arguments, Timeout} | RemainingTests])
		when is_atom (Module), is_atom (Function), is_list (Arguments), is_integer (Timeout), (Timeout > 0) ->
	ok = error_logger:info_report ([lists:flatten (io_lib:format ("Testing `~w:~w (~w)`...", [Module, Function, Arguments]))]),
	Token = erlang:make_ref (),
	Slave = erlang:spawn_link (
			fun () ->
				false = erlang:process_flag (trap_exit, true),
				ok = erlang:apply (Module, Function, Arguments),
				exit ({succeeded, Token})
			end),
	receive
		{'EXIT', Slave, {succeeded, Token}} ->
			execute_tests (RemainingTests);
		{'EXIT', Slave, Reason} ->
			{failed, Test, Reason}
	after Timeout ->
		{failed, Test, timeout}
	end.
