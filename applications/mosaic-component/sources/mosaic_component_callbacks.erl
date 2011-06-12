
-module (mosaic_component_callbacks).

-behaviour (gen_server).


-export ([behaviour_info/1]).
-export ([terminate/0, call/4, cast/4, call_return/2, register/1, acquire/1]).
-export ([start_link/0, stop/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce/2, enforce_ok/1, enforce_ok_1/1]).


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


start_link () ->
	mosaic_process_tools:start_link (gen_server, mosaic_component_callbacks, {local, mosaic_component_callbacks}, defaults).


stop () ->
	try gen_server:call (mosaic_component_callbacks, {mosaic_component_callbacks_internals, stop}) of
		ok -> ok;
		Outcome = {ok, _Reply} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


-record (state, {callback_module, callback_state}).


init ({QualifiedName = {local, mosaic_component_callbacks}, defaults}) ->
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		CallbackModule = enforce_ok_1 (mosaic_component_backend:resolve_callbacks_module ()),
		case erlang:apply (CallbackModule, init, []) of
			{ok, CallbackState} ->
				State = #state{callback_module = CallbackModule, callback_state = CallbackState},
				{ok, State};
			{stop, Reason} ->
				throw ({error, Reason})
		end
	catch throw : {error, StopReason} -> {stop, StopReason} end.


terminate (Reason, _State = #state{callback_module = CallbackModule, callback_state = CallbackState}) ->
	case erlang:apply (CallbackModule, terminate, [Reason, CallbackState]) of
		ok ->
			ok
	end.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call (
			{mosaic_component_callbacks, call, Operation, Inputs, Data}, Sender,
			OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState})
		when is_binary (Operation), is_binary (Data) ->
	case erlang:apply (CallbackModule, handle_call, [Operation, Inputs, Data, Sender, OldCallbackState]) of
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
			{stop, Reason, Error, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_call ({mosaic_component_callbacks_internals, stop}, _Sender, State = #state{}) ->
	{stop, normal, ok, State};
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (
			{mosaic_component_callbacks, cast, Operation, Inputs, Data},
			OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState})
		when is_binary (Operation), is_binary (Data) ->
	case erlang:apply (CallbackModule, handle_cast, [Operation, Inputs, Data, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	case erlang:apply (CallbackModule, handle_info, [Message, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}}
	end.
