
-module (mosaic_component_harness).

-behaviour (gen_fsm).

-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([stop/1, stop/2]).
-export ([execute/2, signal/2]).
-export ([init/1, terminate/3, code_change/4, handle_sync_event/4, handle_event/3, handle_info/3]).
-export ([waiting_execute/3, waiting_execute/2, executing/3, executing/2, executed/3, executed/2]).


start () ->
	start (defaults).

start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName = {local, LocalName}, Configuration)
		when is_atom (LocalName) ->
	gen_fsm:start (QualifiedName, mosaic_component_harness, {QualifiedName, Configuration}, []);
	
start (QualifiedName = noname, Configuration) ->
	gen_fsm:start (mosaic_component_harness, {QualifiedName, Configuration}, []).


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName = {local, LocalName}, Configuration)
		when is_atom (LocalName) ->
	gen_fsm:start_link (QualifiedName, mosaic_component_harness, {QualifiedName, Configuration}, []);
	
start_link (QualifiedName = noname, Configuration) ->
	gen_fsm:start_link (mosaic_component_harness, {QualifiedName, Configuration}, []).


stop (Harness) ->
	stop (Harness, normal).

stop (Harness, Signal)
		when (is_atom (Harness) orelse is_pid (Harness)) ->
	gen_fsm:sync_send_event (Harness, {stop, Signal}).


execute (Harness, Specification)
		when (is_atom (Harness) orelse is_pid (Harness)) ->
	gen_fsm:sync_send_event (Harness, {execute, Specification}).


signal (Harness, Specification)
		when (is_atom (Harness) orelse is_pid (Harness)) ->
	gen_fsm:sync_send_event (Harness, {signal, Specification}).


-record (state, {qualified_name, configuration, port, port_exit_status}).
-record (configuration, {executable, arguments}).
-record (execute_specification, {executable, arguments, environment, working_directory}).
-record (signal_specification, {signal}).


init ({QualifiedName, OriginalConfiguration}) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			case parse_configuration (OriginalConfiguration) of
				{ok, Configuration = #configuration{executable = Executable, arguments = [ArgumentsHead | ArgumentsRest]}} ->
					PortName = {spawn_executable, erlang:binary_to_list (Executable)},
					PortOptions = [
							{arg0, erlang:binary_to_list (ArgumentsHead)},
							{args, [erlang:binary_to_list (Argument) || Argument <- ArgumentsRest]},
							exit_status, use_stdio, binary, {packet, 4}],
					Port = erlang:open_port (PortName, PortOptions),
					State = #state{
							qualified_name = QualifiedName, configuration = Configuration,
							port = Port, port_exit_status = none},
					{ok, waiting_execute, State};
				{error, Reason} ->
					{stop, Reason}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _StateName, _StateData = #state{port = none, port_exit_status = none}) ->
	ok;
	
terminate (_Reason, _StateName, _StateData = #state{port = Port, port_exit_status = none})
		when is_port (Port) ->
	true = try erlang:port_close (Port) catch error : badarg -> true end,
	ok;
	
terminate (_Reason, _StateName, _StateData = #state{port = Port, port_exit_status = PortExitStatus})
		when is_port (Port), ((PortExitStatus =:= none) orelse (is_integer (PortExitStatus) andalso (PortExitStatus >= 0))) ->
	true = try erlang:port_close (Port) catch error : badarg -> true end,
	ok.


code_change (_OldVsn, StateName, StateData, _Data) ->
	{ok, StateName, StateData}.


waiting_execute ({execute, OriginalSpecification}, _Sender, State = #state{port = Port, port_exit_status = none}) ->
	case parse_execute_specification (OriginalSpecification) of
		{ok, Specification} ->
			case trigger_execute (Port, Specification) of
				ok ->
					{reply, ok, executing, State};
				Error = {error, Reason} ->
					{stop, Reason, Error, State}
			end;
		Error = {error, Reason} ->
			{stop, Reason, Error, State}
	end;
	
waiting_execute (Request, Sender, State) ->
	handle_sync_event (Request, Sender, waiting_execute, State).

waiting_execute (Request, OldStateData) ->
	case waiting_execute (Request, undefined, OldStateData) of
		{reply, _Reply, NewStateName, NewStateData} ->
			{next_state, NewStateName, NewStateData};
		{stop, Reason, _Reply, NewStateData} ->
			{stop, Reason, NewStateData}
	end.


executing ({signal, OriginalSpecification}, _Sender, State = #state{port = Port, port_exit_status = none}) ->
	case parse_signal_specification (OriginalSpecification) of
		{ok, Specification} ->
			case trigger_signal (Port, Specification) of
				ok ->
					{reply, ok, executing, State};
				Error = {error, Reason} ->
					{stop, Reason, Error, State}
			end;
		Error = {error, Reason} ->
			{stop, Reason, Error, State}
	end;
	
executing ({internal, port_packet, PacketData}, _Sender, OldState = #state{port = Port, port_exit_status = none})
		when is_binary (PacketData), is_port (Port) ->
	case decode_packet (PacketData) of
		{ok, json, PacketJson} ->
			ok = mosaic_tools:report_info (mosaic_component_harness, executing, port_packet, PacketJson),
			{reply, ok, executing, OldState};
		Error = {error, Reason} ->
			{stop, Reason, Error, OldState}
	end;
	
executing ({internal, port_exit_status, NewPortExitStatus}, _Sender, OldState = #state{port = Port, port_exit_status = none})
		when is_integer (NewPortExitStatus), (NewPortExitStatus >= 0), is_port (Port) ->
	ok = mosaic_tools:report_info (mosaic_component_harness, executing, port_exit_status, NewPortExitStatus),
	{reply, ok, executed, OldState#state{port_exit_status = NewPortExitStatus}};
	
executing (Request, Sender, State) ->
	handle_sync_event (Request, Sender, executing, State).

executing (Request, OldStateData) ->
	case executing (Request, undefined, OldStateData) of
		{reply, _Reply, NewStateName, NewStateData} ->
			{next_state, NewStateName, NewStateData};
		{stop, Reason, _Reply, NewStateData} ->
			{stop, Reason, NewStateData}
	end.


executed ({internal, port_exit_reason, NewPortExitReason}, _Sender, OldState = #state{port = Port, port_exit_status = PortExitStatus})
		when is_port (Port), is_number (PortExitStatus), (PortExitStatus >= 0) ->
	ok = mosaic_tools:report_info (mosaic_component_harness, executed, port_exit_reason, NewPortExitReason),
	{reply, ok, executed, OldState};
	
executed (Request, Sender, State) ->
	handle_sync_event (Request, Sender, executing, State).

executed (Request, OldStateData) ->
	case executed (Request, undefined, OldStateData) of
		{reply, _Reply, NewStateName, NewStateData} ->
			{next_state, NewStateName, NewStateData};
		{stop, Reason, _Reply, NewStateData} ->
			{stop, Reason, NewStateData}
	end.


handle_sync_event ({stop, Signal}, _Sender, _OldStateName, OldStateData = #state{port = Port, port_exit_status = OldPortExitStatus}) ->
	case Signal of
		normal ->
			ok = mosaic_tools:report_info (mosaic_component_harness, handle_sync_event, stop, Signal),
			if
				(Port =:= none), (OldPortExitStatus =:= none) ->
					{stop, normal, ok, OldStateData};
				is_port (Port), (OldPortExitStatus =:= none) ->
					case trigger_terminate (Port) of
						ok ->
							{stop, normal, ok, OldStateData};
							%case wait_port_exit (Port, 2000) of
							%	{ok, NewPortExitStatus, Reason} ->
							%		{stop, Reason, ok, OldStateData#state{port_exit_status = NewPortExitStatus}};
							%	{timeout, exit_status, Reason} ->
							%		{stop, Reason, ok, OldStateData};
							%	{timeout, exit_reason, NewPortExitStatus} ->
							%		Reason = port_timeout,
							%		{stop, Reason, {error, Reason}, OldStateData#state{port_exit_status = NewPortExitStatus}};
							%	timeout ->
							%		Reason = port_timeout,
							%		{stop, Reason, {error, Reason}, OldStateData}
							%end;
						Error = {error, Reason} ->
							{stop, Reason, Error, OldStateData}
					end;
				is_port (Port), is_integer (OldPortExitStatus), (OldPortExitStatus >= 0) ->
					{stop, normal, ok, OldStateData}
			end;
		_ ->
			Reason = {invalid_signal, Signal},
			{stop, Reason, {error, Reason}, OldStateData}
	end;
	
handle_sync_event (Request, _Sender, _StateName, StateData) ->
	Reason = {invalid_request, Request},
	{stop, Reason, {error, Reason}, StateData}.


handle_event (Request, OldStateName, OldStateData) ->
	case handle_sync_event (Request, undefined, OldStateName, OldStateData) of
		{reply, _Reply, NewStateName, NewStateData} ->
			{next_state, NewStateName, NewStateData};
		{stop, Reason, _Reply, NewStateData} ->
			{stop, Reason, NewStateData}
	end.


handle_info ({PeerPort, Callback}, StateName, StateData = #state{port = Port})
		when is_port (PeerPort), is_port (Port), (PeerPort =:= Port) ->
	case Callback of
		{data, PacketData} when is_binary (PacketData) ->
			mosaic_component_harness:StateName ({internal, port_packet, PacketData}, StateData);
		{exit_status, ExitStatus} when is_integer (ExitStatus), (ExitStatus >= 0) ->
			mosaic_component_harness:StateName ({internal, port_exit_status, ExitStatus}, StateData);
		_ ->
			{stop, {invalid_callback, Callback}, StateData}
	end;
	
handle_info ({'EXIT', PeerPort, ExitReason}, StateName, StateData = #state{port = Port})
		when is_port (PeerPort), is_port (Port), (PeerPort =:= Port) ->
	mosaic_component_harness:StateName ({internal, port_exit_reason, ExitReason}, StateData);
	
handle_info (Message, _StateName, StateData) ->
	{stop, {invalid_message, Message}, StateData}.


wait_port_exit (Port, Timeout)
		when is_port (Port), is_integer (Timeout), (Timeout >= 0) ->
	receive
		{Port, {exit_status, ExitStatus}} when is_integer (ExitStatus), (ExitStatus >= 0) ->
			receive
				{'EXIT', Port, ExitReason} ->
					{ok, ExitStatus, ExitReason}
			after Timeout ->
				{timeout, exit_reason, ExitStatus}
			end;
		{'EXIT', Port, ExitReason} ->
			receive
				{Port, {exit_status, ExitStatus}} when is_integer (ExitStatus), (ExitStatus >= 0) ->
					{ok, ExitStatus, ExitReason}
			after Timeout ->
				{timeout, exit_status, ExitReason}
			end
	after Timeout ->
		timeout
	end.


trigger_terminate (Port) ->
	trigger_packet (Port, json, {struct, [{'__type__', terminate}]}).


trigger_execute (
			Port,
			#execute_specification{
					executable = Executable, arguments = Arguments,
					environment = Environment, working_directory = WorkingDirectory}) ->
	trigger_packet (Port, json,
			{struct, [
				{'__type__', execute},
				{executable, Executable},
				{arguments, case Arguments of defaults -> null; _ -> Arguments end},
				{environment, case Environment of defaults -> null; _ -> Environment end},
				{'working-directory', case WorkingDirectory of defaults -> null; _ -> WorkingDirectory end}]}).


trigger_signal (Port, #signal_specification{signal = Signal}) ->
	trigger_packet (Port, json,
			{struct, [
				{'__type__', signal},
				{signal, Signal}]}).


trigger_packet (Port, json, PacketJson)->
	case encode_packet (json, PacketJson) of
		{ok, PacketData} ->
			trigger_packet (Port, binary, PacketData);
		Error = {error, _Reason} ->
			Error
	end;
	
trigger_packet (Port, binary, PacketData)
		when is_binary (PacketData) ->
	true = erlang:port_command (Port, PacketData),
	ok.


validate_configuration (#configuration{executable = Executable, arguments = Arguments}) ->
	try begin
		ok = if
			is_binary (Executable) ->
				ok;
			true ->
				throw ({invalid_executable, Executable})
		end,
		ok = case Arguments of
			defaults ->
				ok;
			_ when is_list (Arguments), length (Arguments) >= 1 ->
				[
					ok = if
						is_binary (Argument) ->
							ok;
						true ->
							throw ({invalid_argument, Argument})
					end
				|| Argument <- Arguments],
				ok;
			_ ->
				throw ({invalid_arguments, Arguments})
		end
	end catch
		throw : Reason ->
			{error, Reason}
	end;
	
validate_configuration (Configuration) ->
	{error, {invalid_configuration, Configuration}}.


parse_configuration (defaults) ->
	Configuration = #configuration{
			executable = <<"./.outputs/gcc/applications-elf/mosaic_component_harness.elf">>,
			arguments = [<<"[mosaic_component_harness]">>]},
	{ok, Configuration};
	
parse_configuration (Configuration = #configuration{}) ->
	case validate_configuration (Configuration) of
		ok ->
			{ok, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_configuration (Configuration) ->
	{error, {invalid_configuration, Configuration}}.


validate_execute_specification (
			#execute_specification{
					executable = Executable, arguments = Arguments,
					environment = Environment, working_directory = WorkingDirectory}) ->
	try begin
		ok = if
			is_binary (Executable) ->
				ok;
			true ->
				throw ({invalid_executable, Executable})
		end,
		ok = case Arguments of
			defaults ->
				ok;
			_ when is_list (Arguments), length (Arguments) >= 1 ->
				[
					ok = if
						is_binary (Argument) ->
							ok;
						true ->
							throw ({invalid_argument, Argument})
					end
				|| Argument <- Arguments],
				ok;
			_ ->
				throw ({invalid_arguments, Arguments})
		end,
		ok = case Environment of
			defaults ->
				ok;
			_ when is_list (Environment) ->
				[
					ok = case EnvironmentPair of
						{EnvironmentName, EnvironmentValue} when is_binary (EnvironmentName), is_binary (EnvironmentValue) ->
							ok;
						_ ->
							throw ({invalid_environment_pair, EnvironmentPair})
					end
				|| EnvironmentPair <- Environment],
				ok;
			_ ->
				throw ({invalid_environment, Environment})
		end,
		ok = case WorkingDirectory of
			defaults ->
				ok;
			_ when is_binary (WorkingDirectory) ->
				ok;
			_ ->
				throw ({invalid_working_directory, WorkingDirectory})
		end,
		ok
	end catch
		throw : Reason ->
			{error, Reason}
	end;
	
validate_execute_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


parse_execute_specification (Specification = #execute_specification{}) ->
	case validate_execute_specification (Specification) of
		ok ->
			{ok, Specification};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_execute_specification (OriginalSpecification)
		when is_list (OriginalSpecification) ->
	DefaultsSpecification = [{arguments, defaults}, {environment, defaults}, {working_directory, defaults}],
	ExtendedSpecification = OriginalSpecification ++ DefaultsSpecification,
	case lists:sort (proplists:get_keys (ExtendedSpecification)) of
		[arguments, environment, executable, working_directory] ->
			try begin
				Executable = case proplists:get_value (executable, ExtendedSpecification) of
					undefined ->
						throw (missing_executable);
					Executable_ when is_binary (Executable_) ->
						Executable_;
					Executable_ ->
						throw ({invalid_executable, Executable_})
				end,
				Arguments = case proplists:get_value (arguments, ExtendedSpecification) of
					defaults ->
						defaults;
					Arguments_ when is_list (Arguments_), length (Arguments_) >= 1 ->
						[
							if
								is_binary (Argument_) ->
									Argument_;
								true ->
									throw ({invalid_argument, Argument_})
							end
						|| Argument_ <- Arguments_];
					Arguments_ ->
						throw ({invalid_arguments, Arguments_})
				end,
				Environment = case proplists:get_value (environment, ExtendedSpecification) of
					defaults ->
						defaults;
					Environment_ when is_list (Environment_) ->
						[
							case EnvironmentPair_ of
								{EnvironmentPairName_, EnvironmentPairValue_}
										when is_binary (EnvironmentPairName_), is_binary (EnvironmentPairValue_) ->
									EnvironmentPair_;
								_ ->
									throw ({invalid_environment_pair, EnvironmentPair_})
							end
						|| EnvironmentPair_ <- Environment_];
					Environment_ ->
						throw ({invalid_environment, Environment_})
				end,
				WorkingDirectory = case proplists:get_value (working_directory, ExtendedSpecification) of
					defaults ->
						defaults;
					WorkingDirectory_ when is_binary (WorkingDirectory_) ->
						WorkingDirectory_;
					WorkingDirectory_ ->
						throw ({invalid_working_directory, WorkingDirectory_})
				end,
				Specification = #execute_specification{
						executable = Executable, arguments = Arguments,
						environment = Environment, working_directory = WorkingDirectory},
				{ok, Specification}
			end catch
				throw : Reason ->
					{error, Reason}
			end;
		_ ->
			{error, {invalid_specification, OriginalSpecification}}
	end;
	
parse_execute_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


validate_signal_specification (#signal_specification{signal = Signal}) ->
	case Signal of
		terminate ->
			ok;
		kill ->
			ok;
		_ ->
			{error, {invalid_signal, Signal}}
	end;
	
validate_signal_specification (Signal) ->
	{error, {invalid_specification, Signal}}.


parse_signal_specification (Specification = #signal_specification{}) ->
	case validate_signal_specification (Specification) of
		ok ->
			{ok, Specification};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_signal_specification (Signal)
		when ((Signal =:= terminate) orelse (Signal =:= kill)) ->
	{ok, #signal_specification{signal = Signal}};
	
parse_signal_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


encode_packet (json, PacketJson) ->
	try begin
		PacketData = erlang:iolist_to_binary (mochijson2:encode (PacketJson)),
		{ok, PacketData}
	end catch
		throw : Reason ->
			{error, {invalid_packet, Reason}};
		exit : Reason ->
			{error, {invalid_packet, Reason}}
	end.

decode_packet (PacketData) ->
	try begin
		PacketJson = mochijson2:decode (PacketData),
		{ok, json, PacketJson}
	end catch
		throw : Reason ->
			{error, {invalid_packet, Reason}};
		exit : Reason ->
			{error, {invalid_packet, Reason}}
	end.
