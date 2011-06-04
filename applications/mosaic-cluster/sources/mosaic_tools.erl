
-module (mosaic_tools).

-export ([resolve_registered/1]).
-export ([ensure_registered/1, ensure_registered/2]).
-export ([enforce_registered/1, enforce_registered/2]).
-export ([start/4, start_link/4]).
-export ([wait/1, wait/2]).
-export ([trace_error/1, trace_error/2, trace_warning/1, trace_warning/2, trace_information/1, trace_information/2]).
-export ([trace_debugging/1, trace_debugging/2]).


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
		when is_atom (LocalName), (is_pid (Process) orelse is_port (Process)) ->
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


trace_debugging (Message) ->
	trace_debugging (Message, []).

trace_debugging (Message, Report) ->
	trace (debugging, Message, Report).

trace_information (Message) ->
	trace_information (Message, []).

trace_information (Message, Report) ->
	trace (information, Message, Report).

trace_warning (Message) ->
	trace_warning (Message, []).

trace_warning (Message, Report) ->
	trace (warning, Message, Report).

trace_error (Message) ->
	trace_error (Message, []).

trace_error (Message, Report) ->
	trace (error, Message, Report).


trace (Level, MessageSpecification, Report)
		when is_tuple (MessageSpecification), (tuple_size (MessageSpecification) > 0), is_list (element (1, MessageSpecification)) ->
	[MessageFormat | MessageArguments] = erlang:tuple_to_list (MessageSpecification),
	Message = lists:flatten (io_lib:format (MessageFormat, MessageArguments)),
	trace (Level, Message, Report);
	
trace (Level, Message, Report)
		when is_list (Message), is_list (Report) ->
	Self = erlang:self (),
	{ok, [_ | StackTrace]} = try throw (stacktrace) catch throw : stacktrace -> {ok, erlang:get_stacktrace ()} end,
	trace (Level, Self, StackTrace, Message, Report).


trace (Level, Process, [LastStackTrace | _], Message, Report) ->
	ok = case Level of
		debugging ->
			ok = error_logger:info_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok;
		information ->
			ok = error_logger:info_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok;
		warning ->
			ok = error_logger:warning_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok;
		error ->
			ok = error_logger:error_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok
	end,
	ok.
