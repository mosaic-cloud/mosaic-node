
-module (mosaic_harness_coders).


-export ([
		validate_frontend_configuration/1, decode_frontend_configuration/2,
		validate_frontend_execute_specification/1, decode_frontend_execute_specification/2,
		validate_frontend_signal_specification/1, decode_frontend_signal_specification/2]).
-export ([
		validate_backend_configuration/1, decode_backend_configuration/2]).
-export ([
		encode_packet/1, decode_packet/1,
		encode_packet_binary/1, decode_packet_binary/1,
		encode_packet_fully/1, decode_packet_fully/1]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


-include ("mosaic_harness.hrl").


validate_frontend_configuration (Configuration) ->
	mosaic_generic_coders:validate_term (Configuration,
				{is_record, #frontend_configuration{
								controller = {is_pid, invalid_controller},
								controller_token = {is_reference, invalid_controller_token},
								executable = {is_binary, invalid_executable},
								argument0 = {is_binary, invalid_argument0},
								arguments = {is_list, {is_binary, invalid_argument}, invalid_arguments}},
							invalid_configuration}).

decode_frontend_configuration (term, OriginalOptions)
		when is_list (OriginalOptions) ->
	DefaultOptions = [{executable, defaults}, {argument0, defaults}, {arguments, defaults}],
	FinalOptions = OriginalOptions ++ DefaultOptions,
	case lists:sort (proplists:get_keys (FinalOptions)) of
		[argument0, arguments, controller, controller_token, executable] ->
			try
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
						{ok, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic_harness.elf">>))};
					undefined ->
						throw ({error, missing_executable});
					Executable_ ->
						throw ({error, {invalid_executable, Executable_}})
				end,
				{ok, Argument0} = case proplists:get_value (argument0, FinalOptions) of
					Argument0_ when is_binary (Argument0_) ->
						{ok, Argument0_};
					defaults ->
						{ok, <<"[mosaic_harness]">>};
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
				Configuration = #frontend_configuration{
						controller = Controller, controller_token = ControllerToken,
						executable = Executable, argument0 = Argument0, arguments = Arguments},
				{ok, Configuration}
			catch
				throw : {error, Reason} -> {error, {invalid_configuration, OriginalOptions, Reason}};
				error : Reason -> {error, {invalid_configuration, OriginalOptions, Reason}}
			end;
		_ ->
			{error, {invalid_configuration, OriginalOptions}}
	end;
	
decode_frontend_configuration (term, Configuration) ->
	{error, {invalid_configuration, Configuration}}.


validate_frontend_execute_specification (Specification) ->
	mosaic_generic_coders:validate_term (Specification,
				{is_record, #frontend_execute_specification{
								executable = {is_binary, invalid_executable},
								argument0 = {'orelse', [{equals, defaults}, is_binary], invalid_argument0},
								arguments = {'orelse', [{equals, defaults},
												{is_list, {is_binary, invalid_argument}, '_'}],
												invalid_arguments},
								environment = {'orelse', [{equals, defaults},
												{is_list,
													{is_tuple, {{is_binary, invalid_name}, {is_binary, invalid_value}}, invalid_environment},
													'_'}],
											invalid_environment},
								working_directory = {'orelse', [{equals, defaults}, is_binary], invalid_working_directory}},
							invalid_specification}).

decode_frontend_execute_specification (term, OriginalOptions)
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
						Environment__ = lists:map (
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
				Specification = #frontend_execute_specification{
						executable = Executable, argument0 = Argument0, arguments = Arguments,
						environment = Environment, working_directory = WorkingDirectory},
				{ok, Specification}
			catch
				throw : {error, Reason} -> {error, {invalid_specification, OriginalOptions, Reason}};
				error : Reason -> {error, {invalid_specification, OriginalOptions, Reason}}
			end;
		_ ->
			{error, {invalid_specification, OriginalOptions}}
	end;
	
decode_frontend_execute_specification (term, Specification) ->
	{error, {invalid_specification, Specification}}.


validate_frontend_signal_specification (Specification) ->
	mosaic_generic_coders:validate_term (Specification,
				{is_record, #frontend_signal_specification{signal = {is_binary, invalid_signal}}, invalid_specification}).

decode_frontend_signal_specification (term, Signal)
		when is_atom (Signal) ->
	{ok, #frontend_signal_specification{signal = erlang:atom_to_binary (Signal, utf8)}};
	
decode_frontend_signal_specification (term, Specification) ->
	{error, {invalid_specification, Specification}}.


validate_backend_configuration (Configuration) ->
	mosaic_generic_coders:validate_term (Configuration,
				{is_record, #backend_configuration{
								controller = {is_pid, invalid_controller},
								controller_token = {is_reference, invalid_controller_token},
								input_descriptor = {is_integer, invalid_input_descriptor},
								output_descriptor = {is_integer, invalid_output_descriptor}},
							invalid_configuration}).

decode_backend_configuration (term, OriginalOptions)
		when is_list (OriginalOptions) ->
	DefaultOptions = [],
	FinalOptions = OriginalOptions ++ DefaultOptions,
	case lists:sort (proplists:get_keys (FinalOptions)) of
		[controller, controller_token, input_descriptor, output_descriptor] ->
			try
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
				{ok, InputDescriptor} = case proplists:get_value (input_descriptor, FinalOptions) of
					InputDescriptor_ when is_integer (InputDescriptor_), (InputDescriptor_ >= 0) ->
						{ok, InputDescriptor_};
					undefined ->
						throw ({error, missing_input_descriptor});
					InputDescriptor_ ->
						throw ({error, {invalid_input_descriptor, InputDescriptor_}})
				end,
				{ok, OutputDescriptor} = case proplists:get_value (output_descriptor, FinalOptions) of
					OutputDescriptor_ when is_integer (OutputDescriptor_), (OutputDescriptor_ >= 0) ->
						{ok, OutputDescriptor_};
					undefined ->
						throw ({error, missing_output_descriptor});
					OutputDescriptor_ ->
						throw ({error, {invalid_output_descriptor, OutputDescriptor_}})
				end,
				Configuration = #backend_configuration{
						controller = Controller, controller_token = ControllerToken,
						input_descriptor = InputDescriptor, output_descriptor = OutputDescriptor},
				{ok, Configuration}
			catch
				throw : {error, Reason} -> {error, {invalid_configuration, OriginalOptions, Reason}};
				error : Reason -> {error, {invalid_configuration, OriginalOptions, Reason}}
			end;
		_ ->
			{error, {invalid_configuration, OriginalOptions}}
	end;
	
decode_backend_configuration (term, Configuration) ->
	{error, {invalid_configuration, Configuration}}.


encode_packet ({exchange, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	{ok, {json, [{<<"__type__">>, <<"exchange">>} | MetaData], Data}};
	
encode_packet ({resources, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	{ok, {json, [{<<"__type__">>, <<"resources">>} | MetaData], Data}};
	
encode_packet ({transcript, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	{ok, {json, [{<<"__type__">>, <<"transcript">>} | MetaData], Data}};
	
encode_packet ({generic, Type, MetaData, Data})
		when is_binary (Type), is_list (MetaData), is_binary (Data) ->
	{ok, {json, [{<<"__type__">>, Type} | MetaData], Data}};
	
encode_packet (
			#frontend_execute_specification{
				executable = Executable, argument0 = Argument0, arguments = Arguments,
				environment = Environment, working_directory = WorkingDirectory}) ->
	{ok, {generic, <<"execute">>, [
				{<<"executable">>, Executable},
				{<<"argument0">>, if (Argument0 =:= defaults) -> null; true -> Argument0 end},
				{<<"arguments">>, if (Arguments =:= defaults) -> null; true -> Arguments end},
				{<<"environment">>, if (Environment =:= defaults) -> null; true -> {struct, Environment} end},
				{<<"working-directory">>, if (WorkingDirectory =:= defaults) -> null; true -> WorkingDirectory end}], <<>>}};
	
encode_packet (#frontend_signal_specification{signal = Signal}) ->
	{ok, {generic, <<"signal">>, [{<<"signal">>, Signal}], <<>>}};
	
encode_packet (#frontend_terminate_specification{}) ->
	{ok, {generic, <<"terminate">>, [], <<>>}};
	
encode_packet (Packet) ->
	{error, {invalid_packet, Packet}}.


decode_packet (Packet = {exit, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	case MetaData of
		[{<<"exit-status">>, ExitStatus}] when is_integer (ExitStatus), (ExitStatus >= 0) ->
			{ok, {exit, ExitStatus}};
		_ ->
			{error, {invalid_packet, Packet}}
	end;
	
decode_packet (Packet = {json, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	try
		{ok, Type, RemainingMetaData} = case lists:keyfind (<<"__type__">>, 1, MetaData) of
			{<<"__type__">>, Type_} ->
				{ok, Type_, lists:sort (lists:keydelete (<<"__type__">>, 1, MetaData))};
			false ->
				throw ({error, {invalid_packet, Packet, missing_type}})
		end,
		case Type of
			<<"exchange">> ->
				{ok, {exchange, RemainingMetaData, Data}};
			<<"resources">> ->
				{ok, {resources, RemainingMetaData, Data}};
			<<"transcript">> ->
				{ok, {transcript, RemainingMetaData, Data}};
			<<"exit">> ->
				{ok, {exit, RemainingMetaData, Data}};
			_ when is_binary (Type) ->
				{ok, {generic, Type, RemainingMetaData, Data}};
			_ ->
				throw ({error, {invalid_packet, Packet, {invalid_type, Type}}})
		end
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet (Packet) ->
	{error, {invalid_packet, Packet}}.


encode_packet_binary (Packet = {json, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	try
		MetaDataBinary = enforce_ok_1 (mosaic_json_coders:encode_json ({struct, MetaData})),
		Payload = <<MetaDataBinary / binary, 0 : 8, Data / binary>>,
		{ok, Payload}
	catch throw : {error, Reason} -> {error, {invalid_packet, Packet, Reason}} end.

decode_packet_binary (Payload)
		when is_binary (Payload) ->
	try
		case binary:split (Payload, <<0 : 8>>) of
			[MetaDataBinary, Data] ->
				case enforce_ok_1 (mosaic_json_coders:decode_json (MetaDataBinary)) of
					{struct, MetaData} ->
						{ok, {json, MetaData, Data}};
					MetaData ->
						throw ({error, {invalid_meta_data, MetaData}})
				end;
			_ ->
				throw ({error, invalid_framing})
		end
	catch throw : {error, Reason} -> {error, {invalid_packet, Payload, Reason}} end.


encode_packet_fully (PacketPayload)
		when is_binary (PacketPayload) ->
	{ok, PacketPayload};
	
encode_packet_fully (Packet = {exchange, _MetaData, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {resources, _MetaData, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {transcript, _MetaData, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {generic, _Type, _MetaData, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = #frontend_execute_specification{}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = #frontend_signal_specification{}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = #frontend_terminate_specification{}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {json, _MetaData, _Data}) ->
	try enforce_ok_1 (encode_packet_binary (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet)
		when is_tuple (Packet), (tuple_size (Packet) >= 1) ->
	{error, {invalid_packet, Packet}}.


decode_packet_fully (Packet = {exchange, _MetaData, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {resources, _MetaData, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {transcript, _MetaData, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {generic, _MetaData, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {exit, _ExitStatus}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {exit, _MetaData, _Data}) ->
	try enforce_ok_1 (decode_packet (Packet)) of
		NewPacket -> decode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet_fully (Packet = {json, _MetaData, _Data}) ->
	try enforce_ok_1 (decode_packet (Packet)) of
		NewPacket -> decode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet_fully (PacketPayload)
		when is_binary (PacketPayload) ->
	try enforce_ok_1 (decode_packet_binary (PacketPayload)) of
		NewPacket -> decode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet_fully (Packet)
		when is_tuple (Packet), (tuple_size (Packet) >= 1) ->
	{error, {invalid_packet, Packet}}.
