
-module (mosaic_harness_frontend).

-behaviour (gen_server).


-export ([start/1, start/2, start_link/1, start_link/2]).
-export ([stop/1, stop/2]).
-export ([execute/2, signal/2, push_packet/2, handoff/3]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_process_tools:start (gen_server, mosaic_harness_frontend, QualifiedName, Configuration).


start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_harness_frontend, QualifiedName, Configuration).


stop (Harness) ->
	stop (Harness, normal).

stop (Harness, Signal)
		when (is_pid (Harness) orelse is_atom (Harness)), is_atom (Signal) ->
	gen_server:call (Harness, {mosaic_harness_frontend, stop, Signal}).


execute (Harness, Specification)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_server:call (Harness, {mosaic_harness_frontend, execute, Specification}).


signal (Harness, Specification)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_server:call (Harness, {mosaic_harness_frontend, signal, Specification}).


push_packet (Harness, Packet)
		when (is_pid (Harness) orelse is_atom (Harness)) ->
	gen_server:call (Harness, {mosaic_harness_frontend, push_packet, Packet}).


handoff (Harness, Controller, ControllerToken)
		when (is_pid (Harness) orelse is_atom (Harness)), is_pid (Controller) ->
	gen_server:call (Harness, {mosaic_harness_frontend, handoff, Controller, ControllerToken}).

-include ("mosaic_harness.hrl").


-record (state, {qualified_name, controller, controller_token, port, port_exit_reason, port_exit_status}).


init ({QualifiedName, Configuration}) ->
	false = erlang:process_flag (trap_exit, true),
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		ok = enforce_ok (mosaic_harness_coders:validate_frontend_configuration (Configuration)),
		#frontend_configuration{
					controller = Controller, controller_token = ControllerToken,
					executable = Executable, argument0 = Argument0, arguments = Arguments
		} = Configuration,
		PortName = {spawn_executable, erlang:binary_to_list (Executable)},
		PortOptions = [
					{arg0, erlang:binary_to_list (Argument0)},
					{args, [erlang:binary_to_list (Argument) || Argument <- Arguments]},
					exit_status, use_stdio, binary, {packet, 4}],
		Port = erlang:open_port (PortName, PortOptions),
		ok = timer:sleep (100),
		State = #state{
					qualified_name = QualifiedName,
					controller = Controller, controller_token = ControllerToken,
					port = Port, port_exit_reason = none, port_exit_status = none},
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
			{mosaic_harness_frontend, stop, Signal}, _Sender,
			State = #state{port = Port})
		when is_port (Port) ->
	try
		ok = case Signal of
			normal -> ok;
			_ -> throw ({error, {invalid_signal, Signal}})
		end,
		ok = enforce_ok (push_packet_internal (Port, enforce_ok_1 (mosaic_harness_coders:encode_packet_fully (#frontend_terminate_specification{})))),
		ok = timer:sleep (100),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, Error, State} end;
	
handle_call (
			{mosaic_harness_frontend, execute, Specification}, _Sender,
			State = #state{port = Port})
		when is_port (Port) ->
	try
		ok = enforce_ok (mosaic_harness_coders:validate_frontend_execute_specification (Specification)),
		ok = enforce_ok (push_packet_internal (Port, enforce_ok_1 (mosaic_harness_coders:encode_packet_fully (Specification)))),
		ok = timer:sleep (100),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, Error, State} end;
	
handle_call (
			{mosaic_harness_frontend, signal, Specification}, _Sender,
			State = #state{port = Port})
		when is_port (Port) ->
	try
		ok = enforce_ok (mosaic_harness_coders:validate_frontend_signal_specification (Specification)),
		ok = enforce_ok (push_packet_internal (Port, enforce_ok_1 (mosaic_harness_coders:encode_packet_fully (Specification)))),
		ok = timer:sleep (100),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, Error, State} end;
	
handle_call (
			{mosaic_harness_frontend, push_packet, Packet}, _Sender,
			State = #state{port = Port})
		when is_port (Port) ->
	try
		ok = enforce_ok (push_packet_internal (Port, enforce_ok_1 (mosaic_harness_coders:encode_packet_fully (Packet)))),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, Error, State} end;
	
handle_call (
			{mosaic_harness_frontend, handoff, Controller, ControllerToken}, _Sender,
			OldState = #state{controller_token = ControllerToken})
		when is_pid (Controller) ->
	NewState = OldState#state{controller = Controller},
	{reply, ok, NewState};
	
handle_call (Request, _Sender, State) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (
			{mosaic_harness_frontend_internals, inbound_packet, Packet = {exchange, _MetaData, _Data}},
			State = #state{controller = Controller, controller_token = ControllerToken}) ->
	Controller ! {mosaic_harness_frontend, ControllerToken, push_packet, Packet},
	{noreply, State};
	
handle_info (
			{mosaic_harness_frontend_internals, inbound_packet, Packet = {resources, _MetaData, _Data}},
			State = #state{controller = Controller, controller_token = ControllerToken}) ->
	Controller ! {mosaic_harness_frontend, ControllerToken, push_packet, Packet},
	{noreply, State};
	
handle_info (
			{mosaic_harness_frontend_internals, inbound_packet, {exit, ExitStatus}},
			State = #state{controller = Controller, controller_token = ControllerToken}) ->
	Controller ! {mosaic_harness_frontend, ControllerToken, exit, ExitStatus},
	{noreply, State};
	
handle_info (
			{mosaic_harness_frontend_internals, inbound_packet, PacketPayload},
			State)
		when is_binary (PacketPayload) ->
	try enforce_ok_1 (mosaic_harness_coders:decode_packet_fully (PacketPayload)) of
		Packet ->
			handle_info ({mosaic_harness_frontend_internals, inbound_packet, Packet}, State)
	catch Error = {error, _Reason} -> {stop, Error, State} end;
	
handle_info (
			{mosaic_harness_frontend_internals, port_exit_reason, PortExitReason},
			OldState = #state{port_exit_reason = none}) ->
	NewState = OldState#state{port = none, port_exit_reason = PortExitReason},
	handle_info ({mosaic_harness_frontend_internals, maybe_stop}, NewState);
	
handle_info (
			{mosaic_harness_frontend_internals, port_exit_status, PortExitStatus},
			OldState = #state{port_exit_status = none})
		when is_integer (PortExitStatus), (PortExitStatus >= 0) ->
	NewState = OldState#state{port_exit_status = PortExitStatus},
	handle_info ({mosaic_harness_frontend_internals, maybe_stop}, NewState);
	
handle_info (
			{mosaic_harness_frontend_internals, maybe_stop},
			State = #state{port_exit_reason = PortExitReason, port_exit_status = PortExitStatus}) ->
	if
		(PortExitReason =:= normal), (PortExitStatus =:= 0) ->
			{stop, normal, State};
		(PortExitReason =:= none); (PortExitStatus =:= none) ->
			{noreply, State};
		(PortExitReason =/= normal); (PortExitStatus > 0) ->
			{stop, {error, {failed_port, PortExitReason, PortExitStatus}}, State}
	end;
	
handle_info ({Port, Callback}, State = #state{port = Port, port_exit_status = none})
		when is_port (Port) ->
	case Callback of
		{data, PacketPayload} when is_binary (PacketPayload) ->
			handle_info ({mosaic_harness_frontend_internals, inbound_packet, PacketPayload}, State);
		{exit_status, ExitStatus} when is_integer (ExitStatus), (ExitStatus >= 0) ->
			handle_info ({mosaic_harness_frontend_internals, port_exit_status, ExitStatus}, State);
		_ ->
			{stop, {invalid_port_callback, Callback}, State}
	end;
	
handle_info ({'EXIT', Port, ExitReason}, State = #state{port = Port})
		when is_port (Port) ->
	handle_info ({mosaic_harness_frontend_internals, port_exit_reason, ExitReason}, State);
	
handle_info (Message, State) ->
	{stop, {invalid_message, Message}, State}.


push_packet_internal (Port, PacketPayload)
		when is_port (Port), is_binary (PacketPayload) ->
	try
		true = erlang:port_command (Port, PacketPayload),
		ok
	catch error : badarg -> {error, failed_pushing_packet} end.
