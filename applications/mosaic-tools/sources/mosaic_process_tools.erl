
-module (mosaic_process_tools).


-export ([resolve_registered/1]).
-export ([ensure_registered/1, ensure_registered/2]).
-export ([enforce_registered/1, enforce_registered/2]).
-export ([start/4, start_link/4]).
-export ([wait/1, wait/2]).


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


wait (Process) ->
	wait (Process, infinity).

wait (Process, Timeout)
		when (is_pid (Process) orelse is_port (Process) orelse is_atom (Process)),
				((Timeout =:= infinity) orelse (is_integer (Timeout) andalso (Timeout >= 0))) ->
	try
		Monitor = erlang:monitor (process, Process),
		ok = receive
			{'DOWN', Monitor, process, Process, noproc} ->
				throw (receive {'EXIT', Process, Reason1_} -> {ok, Reason1_} after 0 -> {error, noproc} end);
			{'DOWN', Monitor, process, Process, noconnection} ->
				throw (receive {'EXIT', Process, Reason1_} -> {ok, Reason1_} after 0 -> {error, noproc} end);
			{_, Monitor, _, _, _} ->
				throw ({error, unexpected_error})
		after 0 ->
			ok
		end,
		{ok, Reason} = receive
			{'DOWN', Monitor, process, Process, Reason2_} ->
				ok = receive {'EXIT', Process, Reason2_} -> ok after 0 -> ok end,
				{ok, Reason2_};
			{_, Monitor, _, _, _} ->
				throw ({error, unexpected_error})
		after Timeout ->
			true = erlang:demonitor (Monitor),
			ok = receive {_, Monitor, _, _, _} -> ok after 0 -> ok end,
			throw ({error, timeout})
		end,
		{ok, Reason}
	catch
		throw : Outcome = {ok, _Reason} ->
			Outcome;
		throw : Error = {error, _Reason} ->
			Error
	end.


resolve_registered (QualifiedName = {local, LocalName})
		when is_atom (LocalName) ->
	case erlang:whereis (LocalName) of
		Process when (is_pid (Process) orelse is_port (Process)) ->
			{ok, Process};
		undefined ->
			{error, {unregistered_process, QualifiedName}}
	end.


ensure_registered (QualifiedName) ->
	ensure_registered (QualifiedName, erlang:self ()).

ensure_registered (QualifiedName = {local, LocalName}, Process)
		when is_atom (LocalName), (is_pid (Process) orelse is_port (Process)) ->
	case erlang:whereis (LocalName) of
		Process ->
			ok;
		OtherProcess when (is_pid (OtherProcess) orelse is_port (OtherProcess)) ->
			{error, {mismatched_process_name, QualifiedName, Process, OtherProcess}};
		undefined ->
			true = erlang:register (LocalName, Process),
			ensure_registered (QualifiedName, Process)
	end;
	
ensure_registered (noname, Process)
		when is_pid (Process) orelse is_port (Process) ->
	ok.


enforce_registered (QualifiedName) ->
	enforce_registered (QualifiedName, erlang:self ()).

enforce_registered (QualifiedName = {local, LocalName}, Process)
		when is_atom (LocalName), (is_pid (Process) orelse is_port (Process)) ->
	case erlang:whereis (LocalName) of
		Process ->
			ok;
		OtherProcess when (is_pid (OtherProcess) orelse is_port (OtherProcess)) ->
			{error, {mismatched_process_name, QualifiedName, Process, OtherProcess}};
		undefined ->
			{error, {unregistered_process, QualifiedName}}
	end;
	
enforce_registered (noname, Process)
		when (is_pid (Process) orelse is_port (Process)) ->
	ok.
