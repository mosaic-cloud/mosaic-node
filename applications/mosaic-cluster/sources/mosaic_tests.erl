
-module (mosaic_tests).


-export ([test_module/1]).
-export ([join/1, join/2]).
-export ([trace/1]).


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
	ok = error_logger:info_report ([lists:flatten (io_lib:format ("Testing `~w:~w ~w`...", [Module, Function, Arguments]))]),
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


join (Process) ->
	join (Process, normal).

join (Process, Reasons)
		when (is_pid (Process) or is_port (Process)), ((Reasons =:= any) orelse (Reasons =:= normal) orelse is_list (Reasons)) ->
	Monitor = erlang:monitor (process, Process),
	ok = receive
		{'DOWN', Monitor, process, Process, noproc} ->
			ok;
		{'DOWN', Monitor, process, Process, noconnection} ->
			ok
	after 0 ->
		ok
	end,
	ReceiveOutcome = receive
		{'DOWN', Monitor, process, Process, Reason_} ->
			{ok, Reason_};
		{'EXIT', Process, Reason_} ->
			{ok, Reason_}
	after 1000 ->
		true = erlang:demonitor (Monitor),
		{error, timeout}
	end,
	case ReceiveOutcome of
		{ok, Reason} ->
			ok = receive
				{'EXIT', Process, Reason} ->
					ok
			after 0 ->
				ok
			end,
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


trace (Process)
		when (is_pid (Process) or is_port (Process)) ->
	Loop = fun (Loop) ->
		receive
			Trace ->
				ok = error_logger:info_report ([lists:flatten (io_lib:format ("Traced...~n~p", [Trace]))]),
				Loop (Loop)
		end
	end,
	Tracer = spawn_link (fun () -> Loop (Loop) end),
	1 = erlang:trace (Process, true, [send, 'receive', procs, {tracer, Tracer}]),
	ok.
