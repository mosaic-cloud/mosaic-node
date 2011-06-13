
-module (mosaic_component_backend).

-behaviour (gen_server).


-export ([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export ([configure/0, resolve_callbacks_module/0]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


start_link () ->
	mosaic_process_tools:start_link (gen_server, mosaic_component_backend, {local, mosaic_component_backend}, defaults).


-record (state, {callbacks, harness, harness_token, pending_senders}).
-record (pending_sender, {correlation, sender}).


init ({QualifiedName = {local, mosaic_component_backend}, defaults}) ->
	false = erlang:process_flag (trap_exit, true),
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		Callbacks = enforce_ok_1 (mosaic_process_tools:resolve_registered ({local, mosaic_component_callbacks})),
		true = erlang:link (Callbacks),
		HarnessInputDescriptor = enforce_ok_1 (mosaic_generic_coders:application_env_get (harness_input_descriptor, mosaic_component,
					{validate, {is_integer, invalid_harness_input_descriptor}}, {error, missing_harness_input_descriptor})),
		HarnessOutputDescriptor = enforce_ok_1 (mosaic_generic_coders:application_env_get (harness_output_descriptor, mosaic_component,
					{validate, {is_integer, invalid_harness_output_descriptor}}, {error, missing_harness_output_descriptor})),
		HarnessToken = erlang:make_ref (),
		HarnessConfiguration = enforce_ok_1 (mosaic_harness_coders:decode_backend_configuration (
					term, [{controller, erlang:self ()}, {controller_token, HarnessToken},
								{input_descriptor, HarnessInputDescriptor}, {output_descriptor, HarnessOutputDescriptor}])),
		Harness = enforce_ok_1 (mosaic_harness_backend:start_link ({local, mosaic_component_harness}, HarnessConfiguration)),
		ok = enforce_ok (
					try
						gen_server:call (Callbacks, {mosaic_component_callbacks, init})
					catch exit : {CallReason, {gen_server, call, _}} -> throw ({error, {callbacks_failed, CallReason}}) end),
		State = #state{
					callbacks = Callbacks,
					harness = Harness, harness_token = HarnessToken,
					pending_senders = orddict:new ()},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (Reason, State = #state{}) ->
	ok = mosaic_application_tools:shutdown (),
	ok = mosaic_transcript:trace_information ("terminating mosaic component...", [{reason, Reason}]),
	terminate_1 (Reason, State).

terminate_1 (_Reason, _State = #state{callbacks = none, harness = none}) ->
	ok;
	
terminate_1 (Reason, OldState = #state{callbacks = Callbacks})
		when is_pid (Callbacks) ->
	ok = try gen_server:call (Callbacks, {mosaic_component_callbacks, stop}) of
		_ -> ok
	catch exit : {_, {gen_server, call, _}} -> ok end,
	ok = case mosaic_process_tools:wait (Callbacks, 5000) of
		{ok, _} ->
			ok;
		{error, _} ->
			true = erlang:exit (Callbacks, kill),
			ok
	end,
	terminate_1 (Reason, OldState#state{callbacks = none});
	
terminate_1 (Reason, OldState = #state{harness = Harness})
		when is_pid (Harness) ->
	_ = mosaic_harness_backend:stop (Harness, normal),
	ok = case mosaic_process_tools:wait (Harness, 5000) of
		{ok, _} ->
			ok;
		{error, _} ->
			true = erlang:exit (Harness, kill),
			ok
	end,
	terminate_1 (Reason, OldState#state{harness = none}).


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_component_callbacks, call, Component, Operation, Inputs, Data}, Sender, State = #state{})
		when is_binary (Component), is_binary (Operation), is_binary (Data) ->
	handle_outbound_call (Component, Operation, Inputs, Data, Sender, State);
	
handle_call ({mosaic_component_callbacks, register, Group}, Sender, State = #state{})
		when is_binary (Group) ->
	handle_register (Group, Sender, State);
	
handle_call ({mosaic_component_callbacks, acquire, Specifications}, Sender, State = #state{}) ->
	handle_acquire (Specifications, Sender, State);
	
handle_call (Request, Sender, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid call request; terminating!", [{request, Request}, {sender, Sender}]),
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast ({mosaic_component_backend, cast, Component, Operation, Inputs, Data}, State = #state{})
		when is_binary (Component), is_binary (Operation), is_binary (Data) ->
	handle_outbound_cast (Component, Operation, Inputs, Data, State);
	
handle_cast (Request, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid cast request; terminating!", [{request, Request}]),
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (
			{mosaic_harness_backend, HarnessToken, push_packet, EncodedPacket},
			State = #state{harness_token = HarnessToken}) ->
	try enforce_ok_1 (mosaic_component_coders:decode_packet_fully (EncodedPacket)) of
		{call, Operation, Correlation, Inputs, Data} ->
			handle_inbound_call (Operation, Correlation, Inputs, Data, State);
		{cast, Operation, Inputs, Data} ->
			handle_inbound_cast (Operation, Inputs, Data, State);
		{call_return, Correlation, Outcome = {ok, _Outputs, _Data}} ->
			handle_outbound_call_return (Correlation, Outcome, State);
		{call_return, Correlation, Error = {error, _Outputs, _Data}} ->
			handle_outbound_call_return (Correlation, Error, State);
		{register_return, Correlation, Outcome = ok} ->
			handle_register_return (Correlation, Outcome, State);
		{register_return, Correlation, Error = {error, _Reason}} ->
			handle_register_return (Correlation, Error, State);
		{acquire_return, Correlation, Outcome = {ok, _Descriptors}} ->
			handle_acquire_return (Correlation, Outcome, State);
		{acquire_return, Correlation, Error = {error, _Reason}} ->
			handle_acquire_return (Correlation, Error, State);
		Packet ->
			{stop, {error, {invalid_packet, Packet}}, State}
	catch throw : Error = {error, Reason} ->
		ok = mosaic_transcript:trace_error ("failed decoding packet; terminating!", [{packet, EncodedPacket}, {reason, Reason}]),
		{stop, Error, State}
	end;
	
handle_info (
			{mosaic_component_backend_internals, push_packet, Packet},
			State = #state{harness = Harness})
		when is_pid (Harness) ->
	try
		EncodedPacket = enforce_ok_1 (mosaic_component_coders:encode_packet_fully (Packet)),
		ok = enforce_ok (mosaic_harness_backend:push_packet (Harness, EncodedPacket)),
		{noreply, State}
	catch throw : Error = {error, Reason} ->
		ok = mosaic_transcript:trace_error ("failed encoding packet; terminating!", [{packet, Packet}, {reason, Reason}]),
		{stop, Error, State}
	end;
	
handle_info ({'EXIT', Callbacks, CallbacksExitReason}, OldState = #state{callbacks = Callbacks})
		when is_pid (Callbacks) ->
	case CallbacksExitReason of
		normal ->
			{stop, normal, OldState#state{callbacks = none}};
		_ ->
			{stop, {error, {callbacks_failed, CallbacksExitReason}}, OldState#state{callbacks = none}}
	end;
	
handle_info ({'EXIT', Harness, HarnessExitReason}, OldState = #state{harness = Harness})
		when is_pid (Harness) ->
	case HarnessExitReason of
		normal ->
			{stop, normal, OldState#state{harness = none}};
		_ ->
			{stop, {error, {harness_failed, HarnessExitReason}}, OldState#state{harness = none}}
	end;
	
handle_info (Request, State = #state{}) ->
	{stop, {error, {invalid_message, Request}}, State}.


handle_outbound_call (Component, Operation, Inputs, Data, Sender, OldState = #state{}) ->
	{ok, Correlation, NewState} = register_pending_sender (Sender, OldState),
	Packet = {call, Component, Operation, Correlation, Inputs, Data},
	handle_info ({mosaic_component_backend_internals, push_packet, Packet}, NewState).


handle_outbound_call_return (Correlation, Outcome, State = #state{}) ->
	unregister_pending_sender (Correlation, Outcome, State).


handle_outbound_cast (Component, Operation, Inputs, Data, State = #state{}) ->
	Packet = {cast, Component, Operation, Inputs, Data},
	handle_info ({mosaic_component_backend_internals, push_packet, Packet}, State).


handle_inbound_call (Operation, Correlation, Inputs, Data, State = #state{callbacks = Callbacks}) ->
	Self = erlang:self (),
	_ = erlang:spawn_link (
				fun () ->
					{ok, Outcome} = try gen_server:call (Callbacks, {mosaic_component_callbacks, call, Operation, Inputs, Data}) of
						Outcome_ = {ok, _Outputs, _Data} -> {ok, Outcome_};
						Error_ = {error, _Reason, _Data} -> {ok, Error_};
						Error_ = {error, _Reason} -> {ok, Error_};
						Reply_ -> {ok, {error, {invalid_reply, Reply_}}}
					catch exit : {Reason, {gen_server, call, _}} -> {ok, {error, Reason}} end,
					Packet = {call_return, Correlation, Outcome},
					Self ! {mosaic_component_backend_internals, push_packet, Packet},
					true = erlang:unlink (Self),
					true = erlang:exit (normal)
				end),
	{noreply, State}.


handle_inbound_cast (Operation, Inputs, Data, State = #state{callbacks = Callbacks}) ->
	ok = gen_server:cast (Callbacks, {mosaic_component_callbacks, cast, Operation, Inputs, Data}),
	{noreply, State}.


handle_register (Group, Sender, OldState = #state{}) ->
	{ok, Correlation, NewState} = register_pending_sender (Sender, OldState),
	Packet = {register, Group, Correlation},
	handle_info ({mosaic_component_backend_internals, push_packet, Packet}, NewState).


handle_register_return (Correlation, Outcome, State = #state{}) ->
	unregister_pending_sender (Correlation, Outcome, State).


handle_acquire (Specifications, Sender, OldState = #state{}) ->
	{ok, Correlation, NewState} = register_pending_sender (Sender, OldState),
	Packet = {acquire, Specifications, Correlation},
	handle_info ({mosaic_component_backend_internals, push_packet, Packet}, NewState).


handle_acquire_return (Correlation, Outcome, State = #state{}) ->
	unregister_pending_sender (Correlation, Outcome, State).


register_pending_sender (Sender, OldState = #state{pending_senders = OldPendingSenders}) ->
	{ok, Correlation} = mosaic_component_coders:generate_correlation (),
	PendingSender = #pending_sender{correlation = Correlation, sender = Sender},
	NewPendingSenders = orddict:store (Correlation, PendingSender, OldPendingSenders),
	NewState = OldState#state{pending_senders = NewPendingSenders},
	{ok, Correlation, NewState}.


unregister_pending_sender (Correlation, Outcome, OldState = #state{pending_senders = OldPendingSenders}) ->
	case orddict:find (Correlation, OldPendingSenders) of
		{ok, #pending_sender{correlation = Correlation, sender = Sender}} ->
			NewPendingSenders = orddict:erase (Correlation, OldPendingSenders),
			NewState = OldState#state{pending_senders = NewPendingSenders},
			_ = gen_server:reply (Sender, Outcome),
			{noreply, NewState};
		error ->
			ok = mosaic_transcript:trace_error ("received unexpected return; ignoring!", [{correlation, Correlation}, {outcome, Outcome}]),
			{noreply, OldState}
	end.


configure () ->
	try
		CallbacksModule = enforce_ok_1 (resolve_callbacks_module ()),
		ok = enforce_ok (erlang:apply (CallbacksModule, configure, [])),
		ok
	catch throw : Error = {error, _Reason} -> Error end.


resolve_callbacks_module () ->
	try
		CallbacksModule = enforce_ok_1 (mosaic_generic_coders:application_env_get (callbacks, mosaic_component,
					{validate, {is_atom, invalid_callbacks}}, {error, missing_callbacks})),
		{ok, CallbacksModule}
	catch throw : Error = {error, _Reason} -> Error end.
