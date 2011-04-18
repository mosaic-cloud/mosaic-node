
-module (mosaic_tools).

-export ([resolve_registered/1, resolve_or_start_registered/4, ensure_registered/2]).
-export ([report_error/4]).

resolve_registered (Name) when is_atom (Name) ->
	case erlang:whereis (Name) of
		undefined ->
			{error, {unregistered_process_name, Name}};
		Process ->
			{ok, Process}
	end.

resolve_or_start_registered (Name, Module, StartFun, StartArguments) when is_atom (Name), is_atom (Module), is_atom (StartFun), is_list (StartArguments) ->
	case erlang:whereis (Name) of
		undefined ->
			case erlang:apply (Module, StartFun, StartArguments) of
				Outcome = {ok, Process} when is_pid (Process); is_port (Process) ->
					case erlang:whereis (Name) of
						undefined ->
							{error, {unregistered_process_name, Name}};
						Process ->
							Outcome;
						OtherProcess ->
							{error, {mismatched_process_name, Name, Process, OtherProcess}}
					end;
				{ok, OtherProcess} ->
					{error, {invalid_process, OtherProcess}};
				Error = {error, _Reason} ->
					Error;
				Outcome ->
					{error, {invalid_outcome, Outcome}}
			end;
		Process ->
			{ok, Process}
	end.

ensure_registered (Name, Process) when is_atom (Name), is_pid (Process); is_port (Process) ->
	case erlang:whereis (Name) of
		undefined ->
			true = erlang:register (Name, Process),
			ok;
		Process ->
			ok;
		OtherProcess ->
			{error, {mismatched_process, Name, Process, OtherProcess}}
	end.

report_error (Module, Function, ErrorType, ErrorDetails) ->
	ok = error_logger:error_report (mosaic_error, [{source, Module, Function, erlang:self ()}, {error, ErrorType, ErrorDetails}]),
	ok.
