
-module (mosaic_tools).

-export ([resolve_registered/1]).
-export ([ensure_registered/1, ensure_registered/2]).
-export ([enforce_registered/1, enforce_registered/2]).
-export ([report_error/4, report_info/4]).


resolve_registered (QualifiedName = {local, LocalName})
		when is_atom (LocalName) ->
	case erlang:whereis (LocalName) of
		Process when (is_pid (Process) or is_port (Process)) ->
			{ok, Process};
		undefined ->
			{error, {unregistered_process, QualifiedName}}
	end.


ensure_registered (QualifiedName) ->
	ensure_registered (QualifiedName, erlang:self ()).

ensure_registered (QualifiedName = {local, LocalName}, Process)
		when is_atom (LocalName), (is_pid (Process) or is_port (Process)) ->
	case erlang:whereis (LocalName) of
		Process ->
			ok;
		OtherProcess when (is_pid (OtherProcess) or is_port (OtherProcess)) ->
			{error, {mismatched_process_name, QualifiedName, Process, OtherProcess}};
		undefined ->
			true = erlang:register (LocalName, Process),
			ensure_registered (QualifiedName, Process)
	end;
	
ensure_registered (noname, Process)
		when is_pid (Process) or is_port (Process) ->
	ok.


enforce_registered (QualifiedName) ->
	enforce_registered (QualifiedName, erlang:self ()).

enforce_registered (QualifiedName = {local, LocalName}, Process)
		when is_atom (LocalName), (is_pid (Process) or is_port (Process)) ->
	case erlang:whereis (LocalName) of
		Process ->
			ok;
		OtherProcess when (is_pid (OtherProcess) or is_port (OtherProcess)) ->
			{error, {mismatched_process_name, QualifiedName, Process, OtherProcess}};
		undefined ->
			{error, {unregistered_process, QualifiedName}}
	end;
	
enforce_registered (noname, Process)
		when (is_pid (Process) or is_port (Process)) ->
	ok.


report_info (Module, Function, InfoType, InfoDetails) ->
	ok = error_logger:info_report ([{source, Module, Function, erlang:self ()}, {info, InfoType, InfoDetails}]),
	ok.

report_error (Module, Function, ErrorType, ErrorDetails) ->
	ok = error_logger:error_report ([{source, Module, Function, erlang:self ()}, {error, ErrorType, ErrorDetails}]),
	ok.
