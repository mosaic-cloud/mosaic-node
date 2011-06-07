
-module (mosaic_component_harness).

-behaviour (gen_fsm).


-export ([start/1, start/2, start_link/1, start_link/2]).
-export ([stop/1, stop/2]).
-export ([execute/2, signal/2, exchange/2]).
-export ([init/1, terminate/3, code_change/4, handle_sync_event/4, handle_event/3, handle_info/3]).
-export ([waiting_execute/3, waiting_execute/2, executing/3, executing/2]).
-export ([
		validate_configuration/1, parse_configuration/1,
		validate_execute_specification/1, parse_execute_specification/1,
		validate_signal_specification/1, parse_signal_specification/1,
		validate_exchange_specification/1, parse_exchange_specification/1]).


start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_tools:start (gen_fsm, mosaic_component_harness, QualifiedName, Configuration).


start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_tools:start_link (gen_fsm, mosaic_component_harness, QualifiedName, Configuration).


stop (Harness) ->
	stop (Harness, normal).

stop (Harness, Signal)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_fsm:sync_send_event (Harness, {mosaic_component_harness, stop, Signal}).


execute (Harness, Specification)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_fsm:sync_send_event (Harness, {mosaic_component_harness, execute, Specification}).


signal (Harness, Specification)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_fsm:sync_send_event (Harness, {mosaic_component_harness, signal, Specification}).


exchange (Harness, Specification)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_fsm:sync_send_event (Harness, {mosaic_component_harness, exchange, Specification}).


-record (state, {qualified_name, identifier, controller, controller_token, port, port_exit_status}).
-record (configuration, {identifier, controller, controller_token, executable, argument0, arguments}).
-record (execute_specification, {executable, argument0, arguments, environment, working_directory}).
-record (signal_specification, {signal}).
-record (exchange_specification, {meta_data, data}).


init ({QualifiedName, OriginalConfiguration}) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			case parse_configuration (OriginalConfiguration) of
				{ok, #configuration{
						identifier = Identifier, controller = Controller, controller_token = ControllerToken,
						executable = Executable, argument0 = Argument0, arguments = Arguments}} ->
					PortName = {spawn_executable, erlang:binary_to_list (Executable)},
					PortOptions = [
							{arg0, erlang:binary_to_list (Argument0)},
							{args, [erlang:binary_to_list (Argument) || Argument <- Arguments]},
							exit_status, use_stdio, binary, {packet, 4}],
					Port = erlang:open_port (PortName, PortOptions),
					ok = timer:sleep (100),
					State = #state{
							qualified_name = QualifiedName,
							identifier = Identifier, controller = Controller, controller_token = ControllerToken,
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
	ok = timer:sleep (100),
	ok;
	
terminate (_Reason, _StateName, _StateData = #state{port = Port, port_exit_status = PortExitStatus})
		when is_port (Port), ((PortExitStatus =:= none) orelse (is_integer (PortExitStatus) andalso (PortExitStatus >= 0))) ->
	true = try erlang:port_close (Port) catch error : badarg -> true end,
	ok = timer:sleep (100),
	ok.


code_change (_OldVsn, StateName, StateData, _Arguments) ->
	{ok, StateName, StateData}.


waiting_execute ({mosaic_component_harness, execute, OriginalSpecification}, _Sender, State = #state{port = Port, port_exit_status = none})
		when is_port (Port) ->
	case parse_execute_specification (OriginalSpecification) of
		{ok, Specification} ->
			case trigger_execute (Port, Specification) of
				ok ->
					ok = timer:sleep (100),
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


executing ({mosaic_component_harness, signal, OriginalSpecification}, _Sender, State = #state{port = Port, port_exit_status = none})
		when is_port (Port) ->
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
	
executing ({mosaic_component_harness, exchange, OriginalSpecification}, _Sender, State = #state{port = Port, port_exit_status = none})
		when is_port (Port) ->
	case parse_exchange_specification (OriginalSpecification) of
		{ok, Specification} ->
			case trigger_exchange (Port, Specification) of
				ok ->
					{reply, ok, executing, State};
				Error = {error, Reason} ->
					{stop, Reason, Error, State}
			end;
		Error = {error, Reason} ->
			{stop, Reason, Error, State}
	end;
	
executing (
			{mosaic_component_harness_internals, port_packet, PacketPayload}, _Sender,
			OldState = #state{controller = Controller, controller_token = ControllerToken, port = Port, port_exit_status = none})
		when is_binary (PacketPayload), is_port (Port), is_pid (Controller), is_reference (ControllerToken) ->
	case decode_packet (PacketPayload) of
		{ok, json, {PacketMetaData, PacketData}} ->
			case lists:sort (PacketMetaData) of
				[{<<"__type__">>, <<"exchange">>} | OtherMetaData] ->
					Controller ! {mosaic_component_harness, exchange, ControllerToken, {OtherMetaData, PacketData}},
					{reply, ok, executing, OldState};
				[{<<"__type__">>, <<"resources">>} | OtherMetaData] ->
					handle_resources (OtherMetaData, PacketData, executing, OldState);
				[{<<"__type__">>, <<"exit">>}, {<<"exit-status">>, ExitStatus}] when PacketData =:= <<"">> ->
					Controller ! {mosaic_component_harness, exit, ControllerToken, ExitStatus},
					{reply, ok, waiting_execute, OldState};
				_ ->
					Reason = {invalid_packet, {PacketMetaData, PacketData}},
					{stop, Reason, {error, Reason}, OldState}
			end;
		Error = {error, Reason} ->
			{stop, Reason, Error, OldState}
	end;
	
executing (Request, Sender, State) ->
	handle_sync_event (Request, Sender, executing, State).

executing (Request, OldStateData) ->
	case executing (Request, undefined, OldStateData) of
		{reply, _Reply, NewStateName, NewStateData} ->
			{next_state, NewStateName, NewStateData};
		{stop, Reason, _Reply, NewStateData} ->
			{stop, Reason, NewStateData}
	end.


handle_sync_event (
			{mosaic_component_harness, stop, Signal}, _Sender,
			OldStateName, OldStateData = #state{port = Port, port_exit_status = OldPortExitStatus}) ->
	case Signal of
		normal ->
			if
				(Port =:= none), (OldPortExitStatus =:= none) ->
					{stop, normal, ok, OldStateData};
				is_port (Port), (OldPortExitStatus =:= none) ->
					case trigger_terminate (Port) of
						ok ->
							{reply, ok, OldStateName, OldStateData};
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
	
handle_sync_event (
			{mosaic_component_harness_internals, port_exit_status, PortExitStatus}, _Sender,
			OldStateName, OldStateData = #state{port = Port, port_exit_status = none})
		when is_integer (PortExitStatus), (PortExitStatus >= 0), is_port (Port) ->
	NewStateData = OldStateData#state{port_exit_status = PortExitStatus},
	case OldStateName of
		waiting_execute when (PortExitStatus =:= 0) ->
			{stop, normal, ok, NewStateData};
		executing when (PortExitStatus =:= 0) ->
			{stop, normal, ok, NewStateData};
		waiting_execute ->
			{stop, port_exit, ok, NewStateData};
		executing ->
			{stop, port_exit, ok, NewStateData}
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
		{data, PacketPayload} when is_binary (PacketPayload) ->
			erlang:apply (mosaic_component_harness, StateName, [{mosaic_component_harness_internals, port_packet, PacketPayload}, StateData]);
		{exit_status, ExitStatus} when is_integer (ExitStatus), (ExitStatus >= 0) ->
			erlang:apply (mosaic_component_harness, StateName, [{mosaic_component_harness_internals, port_exit_status, ExitStatus}, StateData]);
		_ ->
			{stop, {invalid_callback, Callback}, StateData}
	end;
	
handle_info ({'EXIT', PeerPort, ExitReason}, StateName, StateData = #state{port = Port})
		when is_port (PeerPort), is_port (Port), (PeerPort =:= Port) ->
	erlang:apply (mosaic_component_harness, StateName, [{mosaic_component_harness_internals, port_exit_reason, ExitReason}, StateData]);
	
handle_info (Message, _StateName, StateData) ->
	{stop, {invalid_message, Message}, StateData}.


%wait_port_exit (Port, Timeout)
%		when is_port (Port), is_integer (Timeout), (Timeout >= 0) ->
%	receive
%		{Port, {exit_status, ExitStatus}} when is_integer (ExitStatus), (ExitStatus >= 0) ->
%			receive
%				{'EXIT', Port, ExitReason} ->
%					{ok, ExitStatus, ExitReason}
%			after Timeout ->
%				{timeout, exit_reason, ExitStatus}
%			end;
%		{'EXIT', Port, ExitReason} ->
%			receive
%				{Port, {exit_status, ExitStatus}} when is_integer (ExitStatus), (ExitStatus >= 0) ->
%					{ok, ExitStatus, ExitReason}
%			after Timeout ->
%				{timeout, exit_status, ExitReason}
%			end
%	after Timeout ->
%		timeout
%	end.


handle_resources (MetaData, Data, executing, StateData = #state{identifier = Identifier, port = Port}) ->
	case MetaData of
		[{<<"action">>, <<"acquire">>}, {<<"correlation">>, Correlation}, {<<"resources">>, ResourcesSpecification}]
				when is_binary (Correlation), (byte_size (Correlation) =:= 40), (Data =:= <<>>) ->
			case mosaic_component_resources:acquire (Identifier, Port, json, ResourcesSpecification) of
				{ok, ResourcesDescriptor} ->
					trigger_packet (Port, json,
							{[
								{<<"__type__">>, <<"resources">>}, {<<"action">>, <<"return">>}, {<<"correlation">>, Correlation},
								{<<"ok">>, true}, {<<"resources">>, ResourcesDescriptor}]}),
					{reply, ok, executing, StateData};
				Error = {error, Reason} ->
					trigger_packet (Port, json,
							{[
								{<<"__type__">>, <<"resources">>}, {<<"action">>, <<"return">>}, {<<"correlation">>, Correlation},
								{<<"ok">>, false}, {<<"error">>, mosaic_webmachine:format_term (Reason)}]}),
					{reply, Error, executing, StateData}
			end;
		_ ->
			Reason = {invalid_packet, {MetaData, Data}},
			{stop, Reason, {error, Reason}, StateData}
	end.


trigger_terminate (Port) ->
	trigger_packet (Port, json,
			{[
				{'__type__', terminate}]}).


trigger_execute (
			Port,
			#execute_specification{
					executable = Executable, argument0 = Argument0, arguments = Arguments,
					environment = Environment, working_directory = WorkingDirectory}) ->
	trigger_packet (Port, json,
			{[
				{'__type__', execute},
				{executable, Executable},
				{argument0, if Argument0 =:= defaults -> null; true -> Argument0 end},
				{arguments, if Arguments =:= defaults -> null; true -> Arguments end},
				{environment, if Environment =:= defaults -> null; true -> Environment end},
				{'working-directory', if WorkingDirectory =:= defaults -> null; true -> WorkingDirectory end}]}).


trigger_signal (Port, #signal_specification{signal = Signal}) ->
	trigger_packet (Port, json,
			{[
				{'__type__', signal},
				{signal, Signal}]}).


trigger_exchange (Port, #exchange_specification{meta_data = MetaData, data = Data}) ->
	trigger_packet (Port, json,
			{
				[
					{'__type__', exchange} |
					MetaData],
				Data}).


trigger_packet (Port, json, {PacketMetaData}) ->
	trigger_packet (Port, json, {PacketMetaData, <<>>});
	
trigger_packet (Port, json, Packet = {PacketMetaData, PacketData})
		when is_list (PacketMetaData), is_binary (PacketData) ->
	case encode_packet (json, Packet) of
		{ok, PacketPayload} ->
			trigger_packet (Port, binary, PacketPayload);
		Error = {error, _Reason} ->
			Error
	end;
	
trigger_packet (Port, binary, PacketPayload)
		when is_binary (PacketPayload) ->
	true = erlang:port_command (Port, PacketPayload),
	ok.


validate_configuration (
			Configuration = #configuration{
					identifier = Identifier, controller = Controller, controller_token = ControllerToken,
					executable = Executable, argument0 = Argument0, arguments = Arguments}) ->
	try
		ok = if
			is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
				ok;
			true ->
				throw ({error, {invalid_identifier, Identifier}})
		end,
		ok = if
			is_pid (Controller) ->
				ok;
			true ->
				throw ({error, {invalid_controller, Controller}})
		end,
		ok = if
			is_reference (ControllerToken) ->
				ok;
			true ->
				throw ({error, {invalid_controller_token, ControllerToken}})
		end,
		ok = if
			is_binary (Executable) ->
				ok;
			true ->
				throw ({error, {invalid_executable, Executable}})
		end,
		ok = if
			is_binary (Argument0) ->
				ok;
			true ->
				throw ({error, {invalid_argument0, Argument0}})
		end,
		ok = if
			is_list (Arguments) ->
				ok = lists:foreach (
						fun
							(Argument) when is_binary (Argument) -> ok;
							(Argument) -> throw ({error, {invalid_argument, Argument}})
						end,
						Arguments),
				ok;
			true ->
				throw ({error, {invalid_arguments, Arguments}})
		end,
		ok
	catch
		throw : Error = {error, _Reason} ->
			Error;
		error : _ ->
			{error, {invalid_configuration, Configuration}}
	end;
	
validate_configuration (Configuration) ->
	{error, {invalid_configuration, Configuration}}.


parse_configuration (OriginalOptions)
		when is_list (OriginalOptions) ->
	DefaultOptions = [{identifier, undefined}, {executable, defaults}, {argument0, defaults}, {arguments, defaults}],
	FinalOptions = OriginalOptions ++ DefaultOptions,
	case lists:sort (proplists:get_keys (FinalOptions)) of
		[argument0, arguments, controller, controller_token, executable, identifier] ->
			try
				{ok, Identifier} = case proplists:get_value (identifier, FinalOptions) of
					Identifier_ when is_binary (Identifier_), (bit_size (Identifier_) =:= 160) ->
						{ok, Identifier_};
					undefined ->
						throw ({error, missing_identifier});
					Identifier_ ->
						throw ({error, {invalid_identifier, Identifier_}})
				end,
				{ok, Controller} = case proplists:get_value (controller, FinalOptions) of
					Controller_ when is_pid (Controller_) ->
						{ok, Controller_};
					undefined ->
						throw ({error, missing_controller});
					Controller_ ->
						throw ({error, {invalid_controller, Controller_}})
				end,
				{ok, ControllerToken} = case proplists:get_value (controller_token, FinalOptions) of
					ControllerToken_ when is_reference (ControllerToken_) ->
						{ok, ControllerToken_};
					undefined ->
						throw ({error, missing_controller_token});
					ControllerToken_ ->
						throw ({error, {invalid_controller_token, ControllerToken_}})
				end,
				{ok, Executable} = case proplists:get_value (executable, FinalOptions) of
					Executable_ when is_binary (Executable_) ->
						{ok, Executable_};
					defaults ->
						{ok, <<"./.outputs/gcc/applications-elf/mosaic_component_harness.elf">>};
					undefined ->
						throw ({error, missing_executable});
					Executable_ ->
						throw ({error, {invalid_executable, Executable_}})
				end,
				{ok, Argument0} = case proplists:get_value (argument0, FinalOptions) of
					Argument0_ when is_binary (Argument0_) ->
						{ok, Argument0_};
					defaults ->
						{ok, <<"[mosaic_component_harness]">>};
					undefined ->
						throw ({error, missing_argument0});
					Argument0_ ->
						throw ({error, {invalid_argument0, Argument0_}})
				end,
				{ok, Arguments} = case proplists:get_value (arguments, FinalOptions) of
					Arguments_ when is_list (Arguments_) ->
						ok = lists:foreach (
								fun
									(Argument) when is_binary (Argument) -> ok;
									(Argument) -> throw ({error, {invalid_argument, Argument}})
								end,
								Arguments_),
						{ok, Arguments_};
					defaults ->
						{ok, []};
					undefined ->
						throw ({error, missing_arguments});
					Arguments_ ->
						throw ({error, {invalid_arguments, Arguments_}})
				end,
				Configuration = #configuration{
						identifier = Identifier, controller = Controller, controller_token = ControllerToken,
						executable = Executable, argument0 = Argument0, arguments = Arguments},
				{ok, Configuration}
			catch
				throw : Error = {error, _Reason} ->
					Error;
				error : _ ->
					{error, {invalid_configuration, OriginalOptions}}
			end;
		_ ->
			{error, {invalid_configuration, OriginalOptions}}
	end;
	
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
			Specification = #execute_specification{
					executable = Executable, argument0 = Argument0, arguments = Arguments,
					environment = Environment, working_directory = WorkingDirectory}) ->
	try
		ok = if
			is_binary (Executable) ->
				ok;
			true ->
				throw ({error, {invalid_executable, Executable}})
		end,
		ok = if
			is_binary (Argument0) ->
				ok;
			Argument0 =:= defaults ->
				ok;
			true ->
				throw ({error, {invalid_argument0, Argument0}})
		end,
		ok = if
			is_list (Arguments) ->
				ok = lists:foreach (
						fun
							(Argument) when is_binary (Argument) -> ok;
							(Argument) -> throw ({error, {invalid_argument, Argument}})
						end,
						Arguments),
				ok;
			Arguments =:= defaults ->
				ok;
			true ->
				throw ({error, {invalid_arguments, Arguments}})
		end,
		ok = if
			is_list (Environment) ->
				ok = list:foreach (
						fun
							(EnvironmentPair = {EnvironmentName, EnvironmentValue})
									when is_binary (EnvironmentName), is_binary (EnvironmentValue) ->
								EnvironmentPair;
							(EnvironmentPair) ->
								throw ({error, {invalid_environment, EnvironmentPair}})
						end,
						Environment),
				ok;
			Environment =:= defaults ->
				ok;
			true ->
				throw ({error, {invalid_environment, Environment}})
		end,
		ok = if
			is_binary (WorkingDirectory) ->
				ok;
			WorkingDirectory =:= defaults ->
				ok;
			true ->
				throw ({error, {invalid_working_directory, WorkingDirectory}})
		end,
		ok
	catch
		throw : Error = {error, _Reason} ->
			Error;
		error : _ ->
			{error, {invalid_specification, Specification}}
	end;
	
validate_execute_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


parse_execute_specification (OriginalOptions)
		when is_list (OriginalOptions) ->
	DefaultOptions = [{argument0, defaults}, {arguments, defaults}, {environment, defaults}, {working_directory, defaults}],
	FinalOptions = OriginalOptions ++ DefaultOptions,
	case lists:sort (proplists:get_keys (FinalOptions)) of
		[argument0, arguments, environment, executable, working_directory] ->
			try
				{ok, Executable} = case proplists:get_value (executable, FinalOptions) of
					Executable_ when is_binary (Executable_) ->
						{ok, Executable_};
					undefined ->
						throw ({error, missing_executable});
					Executable_ ->
						throw ({error, {invalid_executable, Executable_}})
				end,
				{ok, Argument0} = case proplists:get_value (argument0, FinalOptions) of
					Argument0_ when is_binary (Argument0_) ->
						{ok, Argument0_};
					defaults ->
						{ok, defaults};
					undefined ->
						throw ({error, missing_argument0});
					Argument0_ ->
						throw ({error, {invalid_argument0, Argument0_}})
				end,
				{ok, Arguments} = case proplists:get_value (arguments, FinalOptions) of
					Arguments_ when is_list (Arguments_) ->
						Arguments__ = lists:map (
								fun
									(Argument_) when is_binary (Argument_) -> Argument_;
									(Argument_) -> throw ({error, {invalid_argument, Argument_}})
								end,
								Arguments_),
						{ok, Arguments__};
					defaults ->
						{ok, defaults};
					undefined ->
						throw ({error, missing_arguments});
					Arguments_ ->
						throw ({error, {invalid_arguments, Arguments_}})
				end,
				{ok, Environment} = case proplists:get_value (environment, FinalOptions) of
					Environment_ when is_list (Environment_) ->
						Environment__ = list:map (
								fun
									(EnvironmentPair_ = {EnvironmentName_, EnvironmentValue_})
											when is_binary (EnvironmentName_), is_binary (EnvironmentValue_) ->
										EnvironmentPair_;
									(EnvironmentPair_) ->
										throw ({error, {invalid_environment, EnvironmentPair_}})
								end,
								Environment_),
						{ok, Environment__};
					defaults ->
						{ok, defaults};
					undefined ->
						throw ({error, missing_environment});
					Environment_ ->
						throw ({invalid_environment, Environment_})
				end,
				{ok, WorkingDirectory} = case proplists:get_value (working_directory, FinalOptions) of
					WorkingDirectory_ when is_binary (WorkingDirectory_) ->
						{ok, WorkingDirectory_};
					defaults ->
						{ok, defaults};
					undefined ->
						throw ({error, missing_working_directory});
					WorkingDirectory_ ->
						throw ({error, {invalid_working_directory, WorkingDirectory_}})
				end,
				Specification = #execute_specification{
						executable = Executable, argument0 = Argument0, arguments = Arguments,
						environment = Environment, working_directory = WorkingDirectory},
				{ok, Specification}
			catch
				throw : Reason ->
					{error, Reason};
				error : _ ->
					{error, {invalid_specification, OriginalOptions}}
			end;
		_ ->
			{error, {invalid_specification, OriginalOptions}}
	end;
	
parse_execute_specification (Specification = #execute_specification{}) ->
	case validate_execute_specification (Specification) of
		ok ->
			{ok, Specification};
		Error = {error, _Reason} ->
			Error
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
	
validate_signal_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


parse_signal_specification (Signal)
		when ((Signal =:= terminate) orelse (Signal =:= kill)) ->
	{ok, #signal_specification{signal = Signal}};
	
parse_signal_specification (Specification = #signal_specification{}) ->
	case validate_signal_specification (Specification) of
		ok ->
			{ok, Specification};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_signal_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


validate_exchange_specification (#exchange_specification{meta_data = MetaData, data = Data}) ->
	case mosaic_webmachine:coerce_json ({struct, MetaData}) of
		{ok, {struct, MetaData}} ->
			if
				is_binary (Data) ->
					ok;
				true ->
					{error, {invalid_data, Data}}
			end;
		Error = {error, _Reason} ->
			Error
	end;
	
validate_exchange_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


parse_exchange_specification ({OriginalMetaData, OriginalData})
		when is_list (OriginalMetaData), is_binary (OriginalData) ->
	case mosaic_webmachine:coerce_json ({struct, OriginalMetaData}) of
		{ok, {struct, MetaData}} ->
			Specification = #exchange_specification{meta_data = MetaData, data = OriginalData},
			{ok, Specification};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_exchange_specification (Specification = #exchange_specification{}) ->
	case validate_exchange_specification (Specification) of
		ok ->
			{ok, Specification};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_exchange_specification (Specification) ->
	{error, {invalid_specification, Specification}}.


encode_packet (json, {PacketMetaData, PacketData})
		when is_list (PacketMetaData), is_binary (PacketData) ->
	try
		PacketMetaDataBinary = erlang:iolist_to_binary (mochijson2:encode ({struct, PacketMetaData})),
		PacketPayload = <<PacketMetaDataBinary / binary, 0 : 8, PacketData / binary>>,
		{ok, PacketPayload}
	catch
		throw : Reason ->
			{error, {invalid_packet, Reason}};
		error : Reason ->
			{error, {invalid_packet, Reason}};
		exit : Reason ->
			{error, {invalid_packet, Reason}}
	end.


decode_packet (PacketPayload)
		when is_binary (PacketPayload) ->
	case binary:split (PacketPayload, <<0 : 8>>) of
		[PacketMetaDataBinary, PacketData] ->
			try
				case mochijson2:decode (PacketMetaDataBinary) of
					{struct, PacketMetaData} ->
						{ok, json, {PacketMetaData, PacketData}};
					PacketMetaData ->
						{error, {invalid_meta_data, PacketMetaData}}
				end
			catch
				throw : Reason ->
					{error, {invalid_packet, Reason}};
				error : Reason ->
					{error, {invalid_packet, Reason}};
				exit : Reason ->
					{error, {invalid_packet, Reason}}
			end;
		_ ->
			{error, invalid_packet_framing}
	end.
