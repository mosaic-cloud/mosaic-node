
-module (mosaic_component_callbacks).

-behaviour (gen_server).


-export ([behaviour_info/1]).
-export ([terminate/0, call/4, cast/4, call_return/2, register/1, acquire/1]).
-export ([call_async/5, register_async/2, acquire_async/2]).
-export ([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export ([configure/1]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


behaviour_info (callbacks) ->
	[
		{init, 0},
		{terminate, 2},
		{handle_call, 5},
		{handle_cast, 4},
		{handle_info, 2}].


terminate () ->
	try gen_server:call (mosaic_component_backend, {mosaic_component_callbacks, terminate}) of
		ok -> ok;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


call (Component, Operation, Inputs, Data)
		when is_binary (Component),  is_binary (Operation), is_binary (Data) ->
	try gen_server:call (mosaic_component_backend, {mosaic_component_callbacks, call, Component, Operation, Inputs, Data}) of
		Outcome = {ok, _Reply, _Data} -> Outcome;
		Error = {error, _Reason, _Data} -> Error;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


cast (Component, Operation, Inputs, Data)
		when is_binary (Component), is_binary (Operation), is_binary (Data) ->
	ok = gen_server:cast (mosaic_component_backend, {mosaic_component_callbacks, cast, Component, Operation, Inputs, Data}).


call_return ({Sender, Reference}, Outcome)
		when (is_pid (Sender) orelse is_atom (Sender)), is_reference (Reference) ->
	_ = gen_server:reply (Sender, Outcome),
	ok.


register (Group)
		when is_binary (Group) ->
	try gen_server:call (mosaic_component_backend, {mosaic_component_callbacks, register, Group}) of
		ok -> ok;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


acquire (ResourceSpecifications) ->
	try gen_server:call (mosaic_component_backend, {mosaic_component_callbacks, acquire, ResourceSpecifications}) of
		Outcome = {ok, _Reply} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


call_async (Component, Operation, Inputs, Data, Token)
		when is_binary (Component), is_binary (Operation), is_binary (Data) ->
	Self = erlang:self (),
	_ = erlang:spawn_link (
				fun () ->
					Outcome = call (Component, Operation, Inputs, Data),
					Self ! {Token, Outcome},
					true = erlang:unlink (Self),
					ok
				end),
	ok.


register_async (Group, Token)
		when is_binary (Group) ->
	Self = erlang:self (),
	_ = erlang:spawn_link (
				fun () ->
					Outcome = register (Group),
					Self ! {Token, Outcome},
					true = erlang:unlink (Self),
					ok
				end),
	ok.


acquire_async (Specifications, Token) ->
	Self = erlang:self (),
	_ = erlang:spawn_link (
				fun () ->
					Outcome = acquire (Specifications),
					Self ! {Token, Outcome},
					true = erlang:unlink (Self),
					ok
				end),
	ok.


start_link () ->
	mosaic_process_tools:start_link (gen_server, mosaic_component_callbacks, {local, mosaic_component_callbacks}, defaults).


-record (state, {qualified_name, callback_module, callback_state}).


init ({QualifiedName = {local, mosaic_component_callbacks}, defaults}) ->
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		State = #state{qualified_name = QualifiedName, callback_module = none, callback_state = undefined},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (Reason, _State = #state{callback_module = CallbackModule, callback_state = CallbackState})
		when (CallbackModule =/= none) ->
	try erlang:apply (CallbackModule, terminate, [Reason, CallbackState]) of
		ok ->
			ok;
		Return ->
			ok = mosaic_transcript:trace_error ("callbacks failed (invalid return)...", [{return, Return}]),
			ok
	catch _ : CaughtReason ->
		ok = mosaic_transcript:trace_error ("callbacks failed...", [{reason, CaughtReason}, {stacktrace, erlang:get_stacktrace ()}]),
		ok
	end.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call (
			{mosaic_component_callbacks, call, Operation, Inputs, Data}, Sender,
			OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState})
		when is_binary (Operation), is_binary (Data), (CallbackModule =/= none) ->
	try erlang:apply (CallbackModule, handle_call, [Operation, Inputs, Data, Sender, OldCallbackState]) of
		{reply, Outcome = {ok, _Outputs, _Data}, NewCallbackState} ->
			{reply, Outcome, OldState#state{callback_state = NewCallbackState}};
		{reply, Error = {error, _Reason, _Data}, NewCallbackState} ->
			{reply, Error, OldState#state{callback_state = NewCallbackState}};
		{reply, Error = {error, _Reason}, NewCallbackState} ->
			{reply, Error, OldState#state{callback_state = NewCallbackState}};
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Outcome = {ok, _Outputs, _Data}, NewCallbackState} ->
			{stop, Reason, Outcome, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Error = {error, _Reason, _Data}, NewCallbackState} ->
			{stop, Reason, Error, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Error = {error, _Reason}, NewCallbackState} ->
			{stop, Reason, Error, OldState#state{callback_state = NewCallbackState}};
		Return ->
			ok = mosaic_transcript:trace_error ("callbacks failed (invalid return)...", [{return, Return}]),
			{stop, {error, {callbacks_failed, {invalid_return, Return}}}, {error, callbacks_failed}, OldState}
	catch _ : CaughtReason ->
		ok = mosaic_transcript:trace_error ("callbacks failed...", [{reason, CaughtReason}, {stacktrace, erlang:get_stacktrace ()}]),
		{stop, {error, {callbacks_failed, CaughtReason}}, OldState}
	end;
	
handle_call ({mosaic_component_callbacks, init}, _Sender, OldState = #state{callback_module = none}) ->
	CallbackModule = enforce_ok_1 (mosaic_component_backend:resolve_callbacks_module ()),
	try erlang:apply (CallbackModule, init, []) of
		{ok, CallbackState} ->
			NewState = OldState#state{callback_module = CallbackModule, callback_state = CallbackState},
			{reply, ok, NewState};
		{stop, StopReason} ->
			ok = mosaic_transcript:trace_error ("callbacks failed...", [{reason, StopReason}]),
			{stop, {error, {callbacks_failed, StopReason}}, OldState};
		Return ->
			ok = mosaic_transcript:trace_error ("callbacks failed (invalid return)...", [{return, Return}]),
			{stop, {error, {callbacks_failed, {invalid_return, Return}}}, {error, callbacks_failed}, OldState}
	catch
		throw : Error = {error, _Reason} ->
			{stop, Error, Error, OldState};
		_ : CaughtReason ->
			ok = mosaic_transcript:trace_error ("callbacks failed...", [{reason, CaughtReason}, {stacktrace, erlang:get_stacktrace ()}]),
			{stop, {error, {callbacks_failed, CaughtReason}}, OldState}
	end;
	
handle_call ({mosaic_component_callbacks, stop}, _Sender, State = #state{}) ->
	{stop, normal, ok, State};
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (
			{mosaic_component_callbacks, cast, Operation, Inputs, Data},
			OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState})
		when is_binary (Operation), is_binary (Data) ->
	try erlang:apply (CallbackModule, handle_cast, [Operation, Inputs, Data, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}};
		Return ->
			ok = mosaic_transcript:trace_error ("callbacks failed (invalid return)...", [{return, Return}]),
			{stop, {error, {callbacks_failed, {invalid_return, Return}}}, OldState}
	catch _ : CaughtReason ->
		ok = mosaic_transcript:trace_error ("callbacks failed...", [{reason, CaughtReason}, {stacktrace, erlang:get_stacktrace ()}]),
		{stop, {error, {callbacks_failed, CaughtReason}}, OldState}
	end;
	
handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	try erlang:apply (CallbackModule, handle_info, [Message, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}};
		Return ->
			ok = mosaic_transcript:trace_error ("callbacks failed (invalid return)...", [{return, Return}]),
			{stop, {error, {callbacks_failed, {invalid_return, Return}}}, OldState}
	catch _ : CaughtReason ->
		ok = mosaic_transcript:trace_error ("callbacks failed...", [{reason, CaughtReason}, {stacktrace, erlang:get_stacktrace ()}]),
		{stop, {error, {callbacks_failed, CaughtReason}}, OldState}
	end.


configure ([]) ->
	ok;
	
configure ([Option | RemainingOptions])  ->
	case configure_1 (Option) of
		ok ->
			configure (RemainingOptions);
		Error = {error, _Reason} ->
			Error
	end.


configure_1 ({load, ApplicationResolver, Dependencies})
		when is_function (ApplicationResolver, 0) ->
	case ApplicationResolver () of
		{ok, Applications} ->
			configure_1 ({load, Applications, Dependencies});
		Error = {error, _Reason} ->
			Error
	end;
	
configure_1 ({load, Applications, Dependencies})
		when is_list (Applications); is_atom (Applications) ->
	mosaic_application_tools:load (Applications, Dependencies);
	
configure_1 ({start, ApplicationResolver, Dependencies})
		when is_function (ApplicationResolver, 0) ->
	case ApplicationResolver () of
		{ok, Applications} ->
			configure_1 ({start, Applications, Dependencies});
		Error = {error, _Reason} ->
			Error
	end;
	
configure_1 ({start, Applications, Dependencies})
		when is_list (Applications); is_atom (Applications) ->
	mosaic_application_tools:start (Applications, Dependencies);
	
configure_1 ({env, Application, Name, Value})
		when is_atom (Application), is_atom (Name) ->
	case application:set_env (Application, Name, Value) of
		ok ->
			ok;
		Error = {error, _Reason} ->
			Error
	end;
	
configure_1 ({identifier, Application})
		when is_atom (Application) ->
	try
		AppEnvIdentifier = enforce_ok_1 (mosaic_generic_coders:application_env_get (identifier, Application,
					{decode, fun mosaic_component_coders:decode_component/1}, {default, undefined})),
		OsEnvIdentifier = enforce_ok_1 (mosaic_generic_coders:os_env_get (mosaic_component_identifier,
					{decode, fun mosaic_component_coders:decode_component/1}, {default, undefined})),
		Identifier = if
			(OsEnvIdentifier =/= undefined) -> OsEnvIdentifier;
			(AppEnvIdentifier =/= undefined) -> AppEnvIdentifier;
			true -> throw ({error, missing_identifier})
		end,
		IdentifierString = erlang:binary_to_list (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))),
		ok = enforce_ok (application:set_env (Application, identifier, IdentifierString)),
		ok
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 ({group, Application})
		when is_atom (Application) ->
	try
		AppEnvGroup = enforce_ok_1 (mosaic_generic_coders:application_env_get (group, Application,
					{decode, fun mosaic_component_coders:decode_group/1}, {default, undefined})),
		OsEnvGroup = enforce_ok_1 (mosaic_generic_coders:os_env_get (mosaic_component_group,
					{decode, fun mosaic_component_coders:decode_group/1}, {default, undefined})),
		Group = if
			(OsEnvGroup =/= undefined) -> OsEnvGroup;
			(AppEnvGroup =/= undefined) -> AppEnvGroup;
			true -> throw ({error, missing_group})
		end,
		GroupString = erlang:binary_to_list (enforce_ok_1 (mosaic_component_coders:encode_group (Group))),
		ok = enforce_ok (application:set_env (Application, group, GroupString)),
		ok
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (harness) ->
	try
		HarnessInputDescriptor = enforce_ok_1 (mosaic_generic_coders:os_env_get (mosaic_component_harness_input_descriptor,
					{decode, fun mosaic_generic_coders:decode_integer/1}, {error, missing_harness_input_descriptor})),
		HarnessOutputDescriptor = enforce_ok_1 (mosaic_generic_coders:os_env_get (mosaic_component_harness_output_descriptor,
					{decode, fun mosaic_generic_coders:decode_integer/1}, {error, missing_harness_output_descriptor})),
		ok = enforce_ok (application:set_env (mosaic_component, harness_input_descriptor, HarnessInputDescriptor)),
		ok = enforce_ok (application:set_env (mosaic_component, harness_output_descriptor, HarnessOutputDescriptor)),
		ok
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Option) ->
	{error, {invalid_configuration_option, Option}}.
