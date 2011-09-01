
-module (mosaic_component_process).

-behaviour (mosaic_process).


-export ([
		init/3, terminate/2, handle_stop/2,
		handle_call/5, handle_cast/4, handle_info/2,
		begin_migration/4, commit_migration/1, rollback_migration/1]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


-include ("mosaic_component_process.hrl").


-record (state, {identifier, status, harness, harness_token, execute, resources, router, inbound_pending_calls, outbound_pending_calls}).
-record (inbound_pending_call, {correlation, sender}).
-record (outbound_pending_call, {reference, correlation}).


init (Disposition, Identifier, OriginalConfiguration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	true = erlang:process_flag (trap_exit, true),
	try
		Configuration = enforce_ok_1 (mosaic_component_process_coders:parse_configuration (Disposition, term, OriginalConfiguration)),
		#configuration{
				harness = HarnessOptions, execute = ExecuteSpecification,
				resources = Resources, router = Router
		} = Configuration,
		Status = if (Disposition =:= create) -> executing; (Disposition =:= migrate) -> pre_migrating_as_target end,
		init (prepare, Identifier, {Status, HarnessOptions, ExecuteSpecification, Resources, Router})
	catch throw : {error, Reason} -> {stop, Reason} end;
	
init (prepare, Identifier, {Status, HarnessOptions, ExecuteSpecification, Resources, Router}) ->
	true = erlang:process_flag (trap_exit, true),
	try
		HarnessToken = erlang:make_ref (),
		HarnessConfiguration = enforce_ok_1 (mosaic_harness_coders:decode_frontend_configuration (
					term, [{controller, erlang:self ()}, {controller_token, HarnessToken} | HarnessOptions])),
		Harness = enforce_ok_1 (mosaic_harness_frontend:start_link (HarnessConfiguration)),
		ok = if
			(ExecuteSpecification =/= migrate) ->
				ok = enforce_ok (mosaic_harness_frontend:execute (Harness, ExecuteSpecification)),
				ok;
			true ->
				ok
		end,
		State = #state{
				identifier = Identifier,
				status = Status,
				harness = Harness, harness_token = HarnessToken,
				execute = ExecuteSpecification,
				resources = Resources, router = Router,
				inbound_pending_calls = orddict:new (), outbound_pending_calls = orddict:new ()},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (_Reason, _State = #state{harness = none}) ->
	ok;
	
terminate (_Reason, _State = #state{harness = Harness})
		when is_pid (Harness) ->
	_ = mosaic_harness_frontend:stop (Harness, normal),
	ok = case mosaic_process_tools:wait (Harness, 5000) of
		{ok, _} ->
			ok;
		{error, _} ->
			true = erlang:exit (Harness, kill),
			ok
	end,
	ok.


handle_stop (normal, State = #state{status = executing}) ->
	{stop, normal, ok, State};
	
handle_stop (normal, State = #state{status = Status})
		when (Status =/= executing) ->
	Error = {error, {invalid_status, Status}},
	{stop, Error, Error, State};
	
handle_stop (Signal, State) ->
	Error = {error, {invalid_signal, Signal}},
	{stop, Error, Error, State}.


handle_call (Operation, Inputs, Data, Sender, State) ->
	handle_inbound_call (Operation, Inputs, Data, Sender, State).


handle_cast (Operation, Inputs, Data, State) ->
	handle_inbound_cast (Operation, Inputs, Data, State).


handle_info (
			{mosaic_harness_frontend, HarnessToken, push_packet, EncodedPacket},
			State = #state{harness_token = HarnessToken}) ->
	try enforce_ok_1 (mosaic_component_coders:decode_packet_fully (EncodedPacket)) of
		{call, Component, Operation, Correlation, Inputs, Data} ->
			handle_outbound_call (Component, Operation, Correlation, Inputs, Data, State);
		{cast, Component, Operation, Inputs, Data} ->
			handle_outbound_cast (Component, Operation, Inputs, Data, State);
		{call_return, Correlation, Outcome = {ok, _Outputs, _Data}} ->
			handle_inbound_call_return (Correlation, Outcome, State);
		{call_return, Correlation, Outcome = {error, _Reason, _Data}} ->
			handle_inbound_call_return (Correlation, Outcome, State);
		{register, Group, Correlation} ->
			handle_register (Group, Correlation, State);
		{acquire, Specification, Correlation} ->
			handle_acquire (Specification, Correlation, State);
		Packet ->
			{stop, {error, {invalid_packet, Packet}}, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, State} end;
	
handle_info (
			{mosaic_component_process_internals, push_packet, Packet},
			State = #state{harness = Harness})
		when is_pid (Harness) ->
	try
		EncodedPacket = enforce_ok_1 (mosaic_component_coders:encode_packet_fully (Packet)),
		ok = enforce_ok (mosaic_harness_frontend:push_packet (Harness, EncodedPacket)),
		{noreply, State}
	catch throw : Error = {error, _Reason} -> {stop, Error, State} end;
	
handle_info (
			{mosaic_harness_frontend, HarnessToken, exit, ExitCode},
			State = #state{status = executing, harness_token = HarnessToken})
		when is_integer (ExitCode), (ExitCode >= 0) ->
	if
		(ExitCode =:= 0) ->
			{stop, normal, State};
		(ExitCode > 0) ->
			{stop, {error, {process_failed, ExitCode}}, State}
	end;
	
handle_info (
			{mosaic_harness_frontend, HarnessToken, exit, ExitCode},
			State = #state{status = Status, harness_token = HarnessToken})
		when is_integer (ExitCode), (ExitCode >= 0), ((Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	{noreply, State};
	
handle_info ({Reference, Outcome}, State)
		when is_reference (Reference) ->
	handle_outbound_call_return (Reference, Outcome, State);
	
handle_info ({'EXIT', Harness, HarnessExitReason}, OldState = #state{harness = Harness})
		when is_pid (Harness) ->
	NewState = OldState#state{harness = none, harness_token = none},
	case HarnessExitReason of
		normal ->
			{noreply, NewState};
		_ ->
			{stop, {error, {harness_failed, HarnessExitReason}}, NewState}
	end;
	
handle_info (Message, State) ->
	{stop, {error, {invalid_message, Message}}, State}.


handle_inbound_call (
			Operation, Inputs, Data, Sender,
			OldState = #state{status = Status, inbound_pending_calls = OldPendingCalls})
		when ((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	{ok, Correlation} = mosaic_component_coders:generate_correlation (),
	Packet = {call, Operation, Correlation, Inputs, Data},
	PendingCall = #inbound_pending_call{correlation = Correlation, sender = Sender},
	NewPendingCalls = orddict:store (Correlation, PendingCall, OldPendingCalls),
	NewState = OldState#state{inbound_pending_calls = NewPendingCalls},
	handle_info ({mosaic_component_process_internals, push_packet, Packet}, NewState);
	
handle_inbound_call (Operation, Inputs, Data, Sender, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_warning ("received unexpected inbound call request; ignoring!", [{operation, Operation}, {inputs, Inputs}, {data, Data}, {sender, Sender}, {status, Status}]),
	_ = gen_server:reply (Sender, {error, invalid_status}),
	{noreply, State}.


handle_inbound_call_return (
			Correlation, Outcome,
			OldState = #state{status = Status, inbound_pending_calls = OldPendingCalls})
		when ((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case orddict:find (Correlation, OldPendingCalls) of
		{ok, #inbound_pending_call{correlation = Correlation, sender = Sender}} ->
			_ = gen_server:reply (Sender, Outcome),
			NewPendingCalls = orddict:erase (Correlation, OldPendingCalls),
			NewState = OldState#state{inbound_pending_calls = NewPendingCalls},
			{noreply, NewState};
		error ->
			ok = mosaic_transcript:trace_error ("received invalid inbound call return; ignoring!", [{correlation, Correlation}, {outcome, Outcome}]),
			{noreply, OldState}
	end;
	
handle_inbound_call_return (Correlation, Outcome, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_warning ("received unexpected inbound call return; ignoring!", [{correlation, Correlation}, {outcome, Outcome}, {status, Status}]),
	{noreply, State}.


handle_inbound_cast (Operation, Inputs, Data, State = #state{status = Status})
		when ((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	Packet = {cast, Operation, Inputs, Data},
	handle_info ({mosaic_component_process_internals, push_packet, Packet}, State);
	
handle_inbound_cast (Operation, Inputs, Data, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_warning ("received unexpected inbound cast request; ignoring!", [{operation, Operation}, {inputs, Inputs}, {data, Data}, {status, Status}]),
	{noreply, State}.


handle_outbound_call (
			Component, Operation, Correlation, Inputs, Data,
			OldState = #state{status = Status, router = Router, outbound_pending_calls = OldPendingCalls})
		when ((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	Reference = erlang:make_ref (),
	Sender = {erlang:self (), Reference},
	PendingCall = #outbound_pending_call{reference = Reference, correlation = Correlation},
	NewPendingCalls = orddict:store (Reference, PendingCall, OldPendingCalls),
	NewState = OldState#state{outbound_pending_calls = NewPendingCalls},
	case mosaic_process_router:call (Router, Component, Operation, Inputs, Data, Sender) of
		ok ->
			{noreply, NewState};
		Error = {error, _Reason} ->
			handle_outbound_call_return (Reference, Error, NewState)
	end;
	
handle_outbound_call (Component, Operation, Correlation, Inputs, Data, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_warning ("received unexpected outbound call request; ignoring!", [{component, Component}, {operation, Operation}, {correlation, Correlation}, {inputs, Inputs}, {data, Data}, {status, Status}]),
	{noreply, State}.


handle_outbound_call_return (
			Reference, Outcome,
			OldState = #state{status = Status, outbound_pending_calls = OldPendingCalls})
		when ((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case orddict:find (Reference, OldPendingCalls) of
		{ok, #outbound_pending_call{reference = Reference, correlation = Correlation}} ->
			Packet = {call_return, Correlation, Outcome},
			NewPendingCalls = orddict:erase (Reference, OldPendingCalls),
			NewState = OldState#state{outbound_pending_calls = NewPendingCalls},
			handle_info ({mosaic_component_process_internals, push_packet, Packet}, NewState);
		error ->
			ok = mosaic_transcript:trace_error ("received invalid outbound call return; ignoring!", [{reference, Reference}, {outcome, Outcome}]),
			{noreply, OldState}
	end;
	
handle_outbound_call_return (Reference, Outcome, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_warning ("received unexpected outbound call return; ignoring!", [{reference, Reference}, {outcome, Outcome}, {status, Status}]),
	{noreply, State}.


handle_outbound_cast (Component, Operation, Inputs, Data, State = #state{status = Status, router = Router})
		when ((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	ok = mosaic_process_router:cast (Router, Component, Operation, Inputs, Data),
	{noreply, State};
	
handle_outbound_cast (Component, Operation, Inputs, Data, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_warning ("received unexpected outbound cast request; ignoring!", [{component, Component}, {operation, Operation}, {inputs, Inputs}, {data, Data}, {status, Status}]),
	{noreply, State}.


handle_register (Group, Correlation, State = #state{router = Router}) ->
	Outcome = mosaic_process_router:register (Router, Group, erlang:self ()),
	Packet = {register_return, Correlation, Outcome},
	handle_info ({mosaic_component_process_internals, push_packet, Packet}, State).


handle_acquire (Specification, Correlation, State = #state{identifier = Identifier, resources = Resources}) ->
	Outcome = mosaic_component_resources:acquire (Resources, Identifier, erlang:self (), Specification),
	Packet = {acquire_return, Correlation, Outcome},
	handle_info ({mosaic_component_process_internals, push_packet, Packet}, State).


begin_migration (source, Configuration, CompletionFun, OldState = #state{status = executing, harness = Harness, execute = ExecuteSpecification}) ->
	case Configuration of
		defaults ->
			case mosaic_harness_frontend:stop (Harness, normal) of
				ok ->
					Self = erlang:self (),
					_ = erlang:spawn_link (
								fun () ->
									_ = mosaic_process_tools:wait (Harness, 60000),
									ok = CompletionFun ({prepared, [{execute, ExecuteSpecification}]}),
									ok = CompletionFun (completed),
									true = erlang:unlink (Self),
									ok
								end),
					NewState = OldState#state{status = migrating_as_source},
					{continue, NewState};
				{error, Reason} ->
					{terminate, Reason, OldState#state{status = migrating_as_source_failed}}
			end;
		_ ->
			{reject, {invalid_configuration, Configuration}, OldState}
	end;
	
begin_migration (target, Configuration, CompletionFun, OldState = #state{status = pre_migrating_as_target}) ->
	try
		{ok, ExecuteSpecification} = case Configuration of
			[{execute, ExecuteSpecification_}] ->
				{ok, ExecuteSpecification_};
			_ ->
				throw ({invalid_configuration, Configuration})
		end,
		ok = case mosaic_harness_coders:validate_frontend_execute_specification (ExecuteSpecification) of
			ok ->
				ok;
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end,
		ok = CompletionFun (completed),
		NewState = OldState#state{status = migrating_as_target, execute = ExecuteSpecification},
		{continue, NewState}
	catch
		throw : {error, Reason} ->
			{terminate, Reason, OldState#state{status = migration_failed}}
	end.


commit_migration (OldState = #state{status = migrating_as_source}) ->
	{continue, OldState#state{status = migrating_as_source_succeeded}};
	
commit_migration (OldState = #state{status = migrating_as_target, harness = Harness, execute = ExecuteSpecification}) ->
	case mosaic_harness_frontend:execute (Harness, ExecuteSpecification) of
		ok ->
			{continue, OldState#state{status = executing}};
		{error, Reason} ->
			{terminate, Reason, OldState#state{status = migrating_as_target_failed}}
	end.


rollback_migration (OldState = #state{status = migrating_as_source}) ->
	{terminate, unsupported, OldState#state{status = migrating_as_source_failed}};
	
rollback_migration (OldState = #state{status = Status}) when (Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_target) ->
	{continue, OldState#state{status = migrating_as_target_failed}}.
