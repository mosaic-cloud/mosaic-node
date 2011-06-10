
-module (mosaic_tests).


-export ([test_module/1, test_scenario/2]).
-export ([enforce_start_outcome/2, enforce_stop_outcome/1, enforce_stop_outcome_and_wait/3]).
-export ([wait/1, wait/2]).
-export ([sleep/1]).
-export ([trace/0, trace/1]).


test_module (Module)
		when is_atom (Module) ->
	try
		DefaultTimeout = 10000,
		ok = mosaic_transcript:trace_debugging ({"testing module `~w`...", Module}),
		ModuleLoaded = erlang:module_loaded (Module),
		ok = if
			ModuleLoaded ->
				ok;
			true ->
				throw ({error, {invalid_module, Module}})
		end,
		ok = mosaic_transcript:trace_debugging ({"collecting module `~w` tests...", Module}),
		Tests = lists:flatten (
				lists:map (
					fun
						({test, [{Function, Arguments}]})
								when is_atom (Function), is_list (Arguments) ->
							[{Module, Function, Arguments, DefaultTimeout}];
						({test, [{Function, Arguments, Timeout}]})
								when is_atom (Function), is_list (Arguments),
										((Timeout =:= infinity) orelse (is_integer (Timeout) andalso (Timeout > 0))) ->
							[{Module, Function, Arguments, Timeout}];
						({test, [Test]}) ->
							throw ({error, {invalid_test, Test}});
						({_Name, _Value}) ->
							[]
				end,
				erlang:apply (Module, module_info, [attributes]))),
		ok = mosaic_transcript:trace_debugging ({"executing module `~w` tests...", Module}),
		ok = execute_tests (Tests),
		ok = mosaic_transcript:trace_debugging ({"succeeded testing module `~w`;", Module}),
		ok
	catch
		throw : Error = {error, {failed_test, {Module, Function, Arguments, _Timeout}, Reason}} ->
			ok = mosaic_transcript:trace_error ({"failed testing module `~w`: failed test `~w:~w ~w`;", Module, Module, Function, Arguments}, [{reason, Reason}]),
			Error;
		throw : Error = {error, {invalid_test, Test}} ->
			ok = mosaic_transcript:trace_error ({"failed testing module `~w`: invalid test;", Module}, [{test, Test}]),
			Error;
		throw : Error = {error, {invalid_module, Module}} ->
			ok = mosaic_transcript:trace_error ({"failed testing module `~w`: invalid module;", Module}),
			Error
	end.


test_scenario (Scenario, Tests)
		when is_atom (Scenario), is_list (Tests) ->
	try
		ok = mosaic_transcript:trace_debugging ({"testing scenario `~w`...", Scenario}),
		ok = mosaic_transcript:trace_debugging ({"executing scenario `~w` tests...", Scenario}),
		ok = execute_tests (Tests),
		ok = mosaic_transcript:trace_debugging ({"succeeded testing scenario `~w`;", Scenario}),
		ok
	catch
		throw : Error = {error, {failed_test, {Module, Function, Arguments, _Timeout}, Reason}} ->
			ok = mosaic_transcript:trace_error ({"failed testing scenario `~w`: failed test `~w:~w ~w`;", Scenario, Module, Function, Arguments}, [{reason, Reason}]),
			Error;
		throw : Error = {error, {invalid_test, Test}} ->
			ok = mosaic_transcript:trace_error ({"failed testing scenario `~w`: invalid test;", Scenario}, [{test, Test}]),
			Error
	end.


execute_tests ([]) ->
	ok;
	
execute_tests ([Test = {Module, Function, Arguments, Timeout} | RemainingTests])
		when is_atom (Module), is_atom (Function), is_list (Arguments),
				((Timeout =:= infinity) orelse (is_integer (Timeout) andalso (Timeout > 0))) ->
	ok = mosaic_transcript:trace_debugging ({"executing test `~w:~w ~w` (timeout ~ws)...", Module, Function, Arguments, if is_integer (Timeout) -> Timeout / 1000; true -> Timeout end}),
	SpawnTime = erlang:now (),
	SlaveToken = erlang:make_ref (),
	SlaveProcess = erlang:spawn_link (
			fun () ->
				false = erlang:process_flag (trap_exit, true),
				receive SlaveToken -> ok after 100 -> erlang:exit (timeout) end,
				Exit = try erlang:apply (Module, Function, Arguments) of
					ok ->
						SlaveToken;
					Error = {error, _Reason} ->
						Error
				catch
					throw : Reason ->
						{caught, throw, Reason, erlang:get_stacktrace ()};
					error : Reason ->
						{caught, error, Reason, erlang:get_stacktrace ()};
					exit : Reason ->
						{caught, exit, Reason, erlang:get_stacktrace ()}
				end,
				erlang:exit (Exit)
			end),
	SlaveMonitor = erlang:monitor (process, SlaveProcess),
	ok = receive {_, SlaveMonitor, _, _, _} -> throw ({error, unexpected_error}) after 0 -> ok end,
	_ = SlaveProcess ! SlaveToken,
	Outcome = receive
		{'DOWN', SlaveMonitor, process, SlaveProcess, SlaveToken} ->
			ok = receive {'EXIT', SlaveProcess, SlaveToken} -> ok after 0 -> ok end,
			ok;
		{'DOWN', SlaveMonitor, process, SlaveProcess, ExitReason} ->
			ok = receive {'EXIT', SlaveProcess, ExitReason} -> ok after 0 -> ok end,
			{error, {failed_test, Test, ExitReason}};
		{_, SlaveMonitor, _, _, _} ->
			{error, unexpected_error}
	after Timeout ->
		true = erlang:demonitor (SlaveMonitor),
		true = erlang:exit (SlaveProcess, kill),
		ok = receive {_, SlaveMonitor, _, _, _} -> ok after 0 -> ok end,
		ok = receive {'EXIT', SlaveProcess, _} -> ok after 0 -> ok end,
		{error, {failed_test, Test, timeout}}
	end,
	JoinTime = erlang:now (),
	Elapsed = timer:now_diff (JoinTime, SpawnTime) / 1000000,
	case Outcome of
		ok ->
			ok = mosaic_transcript:trace_debugging ({"succeeded test `~w:~w ~w` (elapsed ~ws);", Module, Function, Arguments, Elapsed}),
			execute_tests (RemainingTests);
		Error = {error, {failed_test, Test, Reason}} ->
			ok = mosaic_transcript:trace_error ({"failed test `~w:~w ~w` (elapsed ~ws);", Module, Function, Arguments, Elapsed}, [{reason, Reason}]),
			throw (Error)
	end;
	
execute_tests ([Test | _]) ->
	throw ({error, {invalid_test, Test}}).


wait (Process) ->
	wait (Process, normal).

wait (Process, Reasons)
		when (is_pid (Process) or is_port (Process)), ((Reasons =:= any) orelse (Reasons =:= normal) orelse is_list (Reasons)) ->
	case mosaic_process_tools:wait (Process) of
		{ok, Reason} ->
			case Reasons of
				any ->
					{ok, Reason};
				normal ->
					case Reason of
						normal ->
							ok;
						_ ->
							{error, {unexpected_reason, Reason}}
					end;
				_ ->
					case lists:member (Reason, Reasons) of
						true ->
							{ok, Reason};
						false ->
							{error, {unexpected_reason, Reason}}
					end
			end;
		Error = {error, _Reason} ->
			Error
	end.


sleep (Timeout)
		when is_integer (Timeout), (Timeout > 0) ->
	ok = timer:sleep (Timeout),
	ok.


enforce_start_outcome (QualifiedName, Outcome) ->
	case Outcome of
		{ok, Process} when is_pid (Process) ->
			case mosaic_process_tools:enforce_registered (QualifiedName, Process) of
				ok ->
					Outcome;
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


enforce_stop_outcome (Outcome) ->
	case Outcome of
		Outcome = ok ->
			Outcome;
		Outcome = {ok, _Reply} ->
			Outcome;
		Error = {error, _Reason} ->
			Error
	end.


enforce_stop_outcome_and_wait (Process, Reasons, Outcome) ->
	case
		case Outcome of
			ok ->
				ok;
			{ok, _Reply} ->
				ok;
			{error, _Reason1} ->
				Outcome
		end
	of
		ok ->
			case wait (Process, Reasons) of
				{ok, _} ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		Error2 = {error, _Reason2} ->
			Error2
	end.


trace () ->
	trace (erlang:self ()).

trace (Process)
		when (is_pid (Process) or is_port (Process)) ->
	Loop = fun (Loop) ->
		receive
			Trace ->
				ok = mosaic_transcript:trace_debugging ("traced process...", [{process, Process}, {trace, Trace}]),
				Loop (Loop)
		end
	end,
	Tracer = spawn_link (fun () -> Loop (Loop) end),
	1 = erlang:trace (Process, true, [send, 'receive', procs, {tracer, Tracer}]),
	ok.
