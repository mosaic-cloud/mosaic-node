
-module (mosaic_tools).

-export ([resolve_registered/1]).
-export ([ensure_registered/1, ensure_registered/2]).
-export ([enforce_registered/1, enforce_registered/2]).
-export ([start/4, start_link/4]).
-export ([report_error/4, report_info/4]).


start (Type, Module, QualifiedName, Configuration)
		when ((Type =:= gen_server) orelse (Type =:= gen_fsm) orelse (Type =:= gen_event)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))),
				is_atom (Module) ->
	case QualifiedName of
		{local, _LocalName} ->
			case Type of
				gen_server ->
					gen_server:start (QualifiedName, Module, {QualifiedName, Configuration}, []);
				gen_fsm ->
					gen_fsm:start (QualifiedName, Module, {QualifiedName, Configuration}, []);
				gen_event when (Module =:= none), (Configuration =:= void) ->
					gen_event:start (QualifiedName)
			end;
		noname ->
			case Type of
				gen_server ->
					gen_server:start (Module, {QualifiedName, Configuration}, []);
				gen_fsm ->
					gen_fsm:start (Module, {QualifiedName, Configuration}, []);
				gen_event when (Module =:= none), (Configuration =:= void) ->
					gen_event:start ()
			end
	end.


start_link (Type, Module, QualifiedName, Configuration)
		when ((Type =:= gen_server) orelse (Type =:= gen_fsm) orelse (Type =:= gen_event)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))),
				is_atom (Module) ->
	case QualifiedName of
		{local, _LocalName} ->
			case Type of
				gen_server ->
					gen_server:start_link (QualifiedName, Module, {QualifiedName, Configuration}, []);
				gen_fsm ->
					gen_fsm:start_link (QualifiedName, Module, {QualifiedName, Configuration}, []);
				gen_event when (Module =:= none), (Configuration =:= void) ->
					gen_event:start_link (QualifiedName)
			end;
		noname ->
			case Type of
				gen_server ->
					gen_server:start_link (Module, {QualifiedName, Configuration}, []);
				gen_fsm ->
					gen_fsm:start_link (Module, {QualifiedName, Configuration}, []);
				gen_event when (Module =:= none), (Configuration =:= void) ->
					gen_event:start_link ()
			end
	end.


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
