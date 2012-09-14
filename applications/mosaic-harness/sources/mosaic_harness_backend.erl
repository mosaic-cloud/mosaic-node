
-module (mosaic_harness_backend).

-behaviour (gen_server).


-export ([start/1, start/2, start_link/1, start_link/2]).
-export ([stop/1, stop/2]).
-export ([push_packet/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_process_tools:start (gen_server, mosaic_harness_backend, QualifiedName, Configuration).


start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_harness_backend, QualifiedName, Configuration).


stop (Harness) ->
	stop (Harness, normal).

stop (Harness, Signal)
		when (is_pid (Harness) orelse is_atom (Harness)), is_atom (Signal) ->
	gen_server:call (Harness, {mosaic_harness_backend, stop, Signal}).


push_packet (Harness, Packet)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_server:call (Harness, {mosaic_harness_backend, push_packet, Packet}).


-include ("mosaic_harness.hrl").


-record (state, {qualified_name, controller, controller_token, port, port_exit_reason}).


init ({QualifiedName, Configuration}) ->
	false = erlang:process_flag (trap_exit, true),
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		ok = enforce_ok (mosaic_harness_coders:validate_backend_configuration (Configuration)),
		#backend_configuration{
					controller = Controller, controller_token = ControllerToken,
					input_descriptor = InputDescriptor, output_descriptor = OutputDescriptor
		} = Configuration,
		PortName = {fd, InputDescriptor, OutputDescriptor},
		PortOptions = [binary, {packet, 4}],
		Port = erlang:open_port (PortName, PortOptions),
		ok = timer:sleep (100),
		State = #state{
					qualified_name = QualifiedName,
					controller = Controller, controller_token = ControllerToken,
					port = Port, port_exit_reason = none},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (_Reason, _State = #state{port = none}) ->
	ok;
	
terminate (_Reason, _State = #state{port = Port})
		when is_port (Port) ->
	ok = try
		true = erlang:port_close (Port),
		ok
	catch error : badarg -> ok end,
	ok = receive
		{'EXIT', Port, _PortExitReason} ->
			ok
	after 5000 ->
		true = erlang:exit (Port, kill),
		ok
	end,
	ok.


code_change (_OldVsn, State, _Arguments) ->
	{ok, State}.


handle_call (
			{mosaic_harness_backend, stop, Signal}, _Sender,
			State = #state{port = Port})
		when is_port (Port) ->
	try
		ok = case Signal of
			normal -> ok;
			_ -> throw ({error, {invalid_signal, Signal}})
		end,
		try
			true = erlang:port_close (Port),
			ok
		catch error : badarg -> ok end,
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, Error, State} end;
	
handle_call (
			{mosaic_harness_backend, push_packet, Packet}, _Sender,
			State = #state{port = Port})
		when is_port (Port) ->
	try
		ok = enforce_ok (push_packet_internal (Port, enforce_ok_1 (mosaic_harness_coders:encode_packet_fully (Packet)))),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, Error, State} end;
	
handle_call (Request, _Sender, State) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (
			{mosaic_harness_backend_internals, inbound_packet, Packet = {exchange, _MetaData, _Data}},
			State = #state{controller = Controller, controller_token = ControllerToken}) ->
	Controller ! {mosaic_harness_backend, ControllerToken, push_packet, Packet},
	{noreply, State};
	
handle_info (
			{mosaic_harness_backend_internals, inbound_packet, Packet = {resources, _MetaData, _Data}},
			State = #state{controller = Controller, controller_token = ControllerToken}) ->
	Controller ! {mosaic_harness_backend, ControllerToken, push_packet, Packet},
	{noreply, State};
	
handle_info (
			{mosaic_harness_backend_internals, inbound_packet, Packet = {transcript, _MetaData, _Data}},
			State = #state{controller = Controller, controller_token = ControllerToken}) ->
	Controller ! {mosaic_harness_backend, ControllerToken, push_packet, Packet},
	{noreply, State};
	
handle_info (
			{mosaic_harness_backend_internals, inbound_packet, PacketPayload},
			State)
		when is_binary (PacketPayload) ->
	try enforce_ok_1 (mosaic_harness_coders:decode_packet_fully (PacketPayload)) of
		Packet ->
			handle_info ({mosaic_harness_backend_internals, inbound_packet, Packet}, State)
	catch Error = {error, _Reason} -> {stop, Error, State} end;
	
handle_info (
			{mosaic_harness_backend_internals, port_exit_reason, PortExitReason},
			OldState = #state{port_exit_reason = none}) ->
	NewState = OldState#state{port = none, port_exit_reason = PortExitReason},
	handle_info ({mosaic_harness_backend_internals, maybe_stop}, NewState);
	
handle_info (
			{mosaic_harness_backend_internals, maybe_stop},
			State = #state{port_exit_reason = PortExitReason}) ->
	if
		(PortExitReason =:= normal) ->
			{stop, normal, State};
		(PortExitReason =/= normal) ->
			{stop, {error, {failed_port, PortExitReason}}, State}
	end;
	
handle_info ({Port, {data, PacketPayload}}, State = #state{port = Port, port_exit_reason = none})
		when is_port (Port), is_binary (PacketPayload) ->
	handle_info ({mosaic_harness_backend_internals, inbound_packet, PacketPayload}, State);
	
handle_info ({'EXIT', Port, ExitReason}, State = #state{port = Port})
		when is_port (Port) ->
	handle_info ({mosaic_harness_backend_internals, port_exit_reason, ExitReason}, State);
	
handle_info (Message, State) ->
	{stop, {invalid_message, Message}, State}.


push_packet_internal (Port, PacketPayload)
		when is_port (Port), is_binary (PacketPayload) ->
	try
		true = erlang:port_command (Port, PacketPayload),
		ok
	catch error : badarg -> {error, failed_pushing_packet} end.
