
-module (mosaic_component_process).

-behaviour (mosaic_process).


-export ([validate_configuration/2, parse_configuration/3]).
-export ([
		init/3, terminate/2, handle_stop/2,
		handle_call/4, handle_cast/3, handle_info/2,
		begin_migration/4, commit_migration/1, rollback_migration/1]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


-record (state, {identifier, status, harness, harness_token, execute, router, inbound_pending_calls, outbound_pending_calls}).
-record (configuration, {harness, execute, router}).
-record (inbound_pending_call, {correlation, sender}).
-record (outbound_pending_call, {reference, correlation}).


init (Disposition, Identifier, OriginalConfiguration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate))->
	try
		case parse_configuration (Disposition, term, OriginalConfiguration) of
			{ok, #configuration{harness = HarnessOptions, execute = ExecuteSpecification, router = Router}} ->
				Status = if (Disposition =:= create) -> executing; (Disposition =:= migrate) -> pre_migrating_as_target end,
				init (prepare, Identifier, {Status, HarnessOptions, ExecuteSpecification, Router});
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end
	catch
		throw : {error, Reason} ->
			{stop, Reason}
	end;
	
init (prepare, Identifier, {Status, HarnessOptions, ExecuteSpecification, Router}) ->
	true = erlang:process_flag (trap_exit, true),
	try
		HarnessToken = erlang:make_ref (),
		{ok, HarnessConfiguration} = case mosaic_harness_coders:decode_configuration (term, [{controller, erlang:self ()}, {controller_token, HarnessToken} | HarnessOptions]) of
			Outcome1 = {ok, _HarnessConfiguration} ->
				Outcome1;
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end,
		{ok, Harness} = case mosaic_harness_frontend:start_link (HarnessConfiguration) of
			Outcome2 = {ok, _Harness} ->
				Outcome2;
			Error2 = {error, _Reason2} ->
				throw (Error2)
		end,
		ok = if
			(ExecuteSpecification =/= migrate) ->
				ok = case mosaic_harness_frontend:execute (Harness, ExecuteSpecification) of
					ok ->
						ok;
					Error3 = {error, _Reason3} ->
						throw (Error3)
				end;
			true ->
				ok
		end,
		State = #state{
				identifier = Identifier,
				status = Status,
				harness = Harness, harness_token = HarnessToken,
				execute = ExecuteSpecification,
				router = Router,
				inbound_pending_calls = orddict:new (), outbound_pending_calls = orddict:new ()},
		{ok, State}
	catch
		throw : {error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{harness = none}) ->
	ok;
	
terminate (_Reason, _State = #state{harness = Harness})
		when is_pid (Harness) ->
	ok = try _ = mosaic_harness_frontend:stop (Harness, normal), ok catch exit : {noproc, _} -> ok end,
	case mosaic_process_tools:wait (Harness, 5000) of
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
	{reply, {error, {invalid_status, Status}}, State};
	
handle_stop (Signal, State) ->
	{reply, {error, {invalid_signal, Signal}}, State}.


handle_call (Request, RequestData, Sender, State) ->
	handle_info ({mosaic_component_process_internals, inbound_call, Request, RequestData, Sender}, State).


handle_cast (Request, RequestData, State) ->
	handle_info ({mosaic_component_process_internals, inbound_cast, Request, RequestData}, State).


handle_info (
			{mosaic_harness_frontend, HarnessToken, push_packet, {exchange, MetaData, Data}},
			State = #state{status = Status, harness_token = HarnessToken})
		when is_list (MetaData), is_binary (Data),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case lists:sort (MetaData) of
		[{<<"action">>, <<"call">>}, {<<"component">>, Component_}, {<<"correlation">>, Correlation_}, {<<"meta-data">>, Request}]
				when is_binary (Component_), is_binary (Correlation_) ->
			case mosaic_component_coders:decode_component (Component_) of
				{ok, Component} ->
					case mosaic_component_coders:decode_correlation (Correlation_) of
						{ok, Correlation} ->
							handle_info ({mosaic_component_process_internals, outbound_call, Component, Correlation, Request, Data}, State);
						{error, Reason} ->
							ok = mosaic_transcript:trace_error ("received invalid outbound call request: invalid correlation identifier; ignoring!", [{correlation, Correlation_}, {reason, Reason}]),
							{noreply, State}
					end;
				{error, Reason} ->
					ok = mosaic_transcript:trace_error ("received invalid outbound call request: invalid component identifier; ignoring!", [{component, Component_}, {reason, Reason}]),
					{noreply, State}
			end;
		[{<<"action">>, <<"cast">>}, {<<"component">>, Component_}, {<<"meta-data">>, Request}]
				when is_binary (Component_) ->
			case mosaic_component_coders:decode_component (Component_) of
				{ok, Component} ->
					handle_info ({mosaic_component_process_internals, outbound_cast, Component, Request, Data}, State);
				{error, Reason} ->
					ok = mosaic_transcript:trace_error ("received invalid outbound cast request: invalid component identifier; ignoring!", [{component, Component_}, {reason, Reason}]),
					{noreply, State}
			end;
		[{<<"action">>, <<"return">>}, {<<"correlation">>, Correlation_}, {<<"meta-data">>, Reply}]
				when is_binary (Correlation_) ->
			case mosaic_component_coders:decode_correlation (Correlation_) of
				{ok, Correlation} ->
					handle_info ({mosaic_component_process_internals, inbound_return, Correlation, {ok, Reply, Data}}, State);
				{error, Reason} ->
					ok = mosaic_transcript:trace_error ("received invalid inbound call return: invalid correlation identifier; ignoring!", [{correlation, Correlation_}, {reason, Reason}]),
					{noreply, State}
			end;
		[{<<"action">>, <<"register">>}, {<<"correlation">>, Correlation__}, {<<"group">>, Group__}]
				when is_binary (Correlation__), is_binary (Group__) ->
			try
				{ok, Correlation} = case mosaic_component_coders:decode_correlation (Correlation__) of
					{ok, Correlation_} ->
						{ok, Correlation_};
					{error, Reason1} ->
						ok = mosaic_transcript:trace_error ("received invalid register: invalid correlation identifier; ignoring!", [{correlation, Correlation__}, {reason, Reason1}]),
						throw (noreply)
				end,
				{ok, Group} = case mosaic_component_coders:decode_group (Group__) of
					{ok, Group_} ->
						{ok, Group_};
					{error, Reason2} ->
						ok = mosaic_transcript:trace_error ("received invalid register: invalid group identifier; ignoring!", [{grop, Group__}, {reason, Reason2}]),
						throw (noreply)
				end,
				handle_info ({mosaic_component_process_internals, inbound_register, Correlation, Group}, State)
			catch
				throw : noreply ->
					{noreply, State}
			end;
		_ ->
			ok = mosaic_transcript:trace_error ("received invalid exchange meta-data; ignoring!", [{meta_data, MetaData}, {data, Data}]),
			{noreply, State}
	end;
	
handle_info (
			{mosaic_harness_frontend, HarnessToken, exit, ExitCode},
			State = #state{harness_token = HarnessToken})
		when is_integer (ExitCode), (ExitCode >= 0) ->
	{stop, if (ExitCode =:= 0) -> normal; true -> {failed, ExitCode} end, State};
	
handle_info ({Reference, Outcome}, State)
		when is_reference (Reference) ->
	handle_info ({outbound_return, Reference, Outcome}, State);
	
handle_info (
			{mosaic_component_process_internals, inbound_call, Request, RequestData, Sender},
			OldState = #state{status = Status, harness = Harness, inbound_pending_calls = OldPendingCalls})
		when is_binary (RequestData),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	{ok, Correlation} = mosaic_component_coders:generate_correlation (),
	MetaData = [{<<"action">>, <<"call">>}, {<<"correlation">>, enforce_ok_1 (mosaic_component_coders:encode_correlation (Correlation))}, {<<"meta-data">>, Request}],
	PendingCall = #inbound_pending_call{correlation = Correlation, sender = Sender},
	NewPendingCalls = orddict:store (Correlation, PendingCall, OldPendingCalls),
	NewState = OldState#state{inbound_pending_calls = NewPendingCalls},
	case mosaic_harness_frontend:push_packet (Harness, {exchange, MetaData, RequestData}) of
		ok ->
			{noreply, NewState};
		Error = {error, _Reason} ->
			handle_info ({mosaic_component_process_internals, inbound_return, Correlation, Error}, NewState)
	end;
	
handle_info (
			{mosaic_component_process_internals, inbound_call, Request, RequestData, Sender},
			State = #state{status = Status})
		when is_binary (RequestData) ->
	ok = mosaic_transcript:trace_error ("received unexpected inbound call; ignoring!", [{request, Request}, {request_data, RequestData}, {sender, Sender}]),
	_ = gen_server:reply (Sender, {error, {invalid_status, Status}}),
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, inbound_return, Correlation, Outcome},
			OldState = #state{status = Status, inbound_pending_calls = OldPendingCalls})
		when is_binary (Correlation), (bit_size (Correlation) =:= 128),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case orddict:find (Correlation, OldPendingCalls) of
		{ok, #inbound_pending_call{correlation = Correlation, sender = Sender}} ->
			ok = case Outcome of
				{ok, _Reply, ReplyData} when is_binary (ReplyData) ->
					_ = gen_server:reply (Sender, Outcome),
					ok;
				{error, _Reason} ->
					_ = gen_server:reply (Sender, Outcome),
					ok;
				_ ->
					ok = mosaic_transcript:trace_error ("received invalid inbound return: invalid outcome; ignoring!", [{correlation, Correlation}, {outcome, Outcome}]),
					ok
			end,
			NewPendingCalls = orddict:erase (Correlation, OldPendingCalls),
			NewState = OldState#state{inbound_pending_calls = NewPendingCalls},
			{noreply, NewState};
		error ->
			ok = mosaic_transcript:trace_error ("received invalid inbound return: invalid correlation; ignoring!", [{correlation, Correlation}, {outcome, Outcome}]),
			{noreply, OldState}
	end;
	
handle_info (
			{mosaic_component_process_internals, inbound_return, Correlation, _Outcome},
			State = #state{status = _Status})
		when is_binary (Correlation), (bit_size (Correlation) =:= 128) ->
	% !!!!
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, outbound_call, Component, Correlation, Request, RequestData},
			OldState = #state{status = Status, router = Router, outbound_pending_calls = OldPendingCalls})
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (Correlation), (bit_size (Correlation) =:= 128), is_binary (RequestData),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	Reference = erlang:make_ref (),
	Sender = {erlang:self (), Reference},
	PendingCall = #outbound_pending_call{reference = Reference, correlation = Correlation},
	NewPendingCalls = orddict:store (Reference, PendingCall, OldPendingCalls),
	NewState = OldState#state{outbound_pending_calls = NewPendingCalls},
	case mosaic_process_router:call (Router, Sender, Component, Request, RequestData) of
		ok ->
			{noreply, NewState};
		Error = {error, _Reason} ->
			handle_info ({mosaic_component_process_internals, outbound_return, Reference, Error}, NewState)
	end;
	
handle_info (
			{mosaic_component_process_internals, outbound_call, Component, Correlation, _Request, RequestData},
			State = #state{status = _Status})
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (Correlation), (bit_size (Correlation) =:= 128), is_binary (RequestData) ->
	% !!!!
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, outbound_return, Reference, Outcome},
			OldState = #state{status = Status, harness = Harness, outbound_pending_calls = OldPendingCalls})
		when is_reference (Reference),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case orddict:find (Reference, OldPendingCalls) of
		{ok, #outbound_pending_call{reference = Reference, correlation = Correlation}} ->
			ok = case Outcome of
				{ok, Reply, ReplyData} when is_binary (ReplyData) ->
					MetaData = [{<<"action">>, <<"return">>}, {<<"correlation">>, enforce_ok_1 (mosaic_component_coders:encode_correlation (Correlation))}, {<<"meta-data">>, Reply}],
					case mosaic_harness_frontend:push_packet (Harness, {exchange, MetaData, ReplyData}) of
						ok ->
							ok;
						{error, _Reason} ->
							% !!!!
							ok
					end;
				{error, _Reason} ->
					% !!!!
					ok;
				_ ->
					% !!!!
					ok
			end,
			NewPendingCalls = orddict:erase (Correlation, OldPendingCalls),
			NewState = OldState#state{outbound_pending_calls = NewPendingCalls},
			{noreply, NewState};
		error ->
			% !!!!
			{noreply, OldState}
	end;
	
handle_info (
			{mosaic_component_process_internals, outbound_return, Reference, _Outcome},
			State = #state{status = _Status})
		when is_reference (Reference) ->
	% !!!!
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, inbound_cast, Request, RequestData},
			State = #state{status = Status, harness = Harness})
		when is_binary (RequestData),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	MetaData = [{<<"action">>, <<"cast">>}, {<<"meta-data">>, Request}],
	case mosaic_harness_frontend:push_packet (Harness, {exchange, MetaData, RequestData}) of
		ok ->
			{noreply, State};
		_Error = {error, _Reason} ->
			% !!!!
			{noreply, State}
	end;
	
handle_info (
			{mosaic_component_process_internals, inbound_cast, _Request, RequestData},
			State = #state{status = _Status})
		when is_binary (RequestData) ->
	% !!!!
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, outbound_cast, Component, Request, RequestData},
			State = #state{status = Status, router = Router})
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (RequestData),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case mosaic_process_router:cast (Router, Component, Request, RequestData) of
		ok ->
			{noreply, State};
		_Error = {error, _Reason} ->
			% !!!!
			{noreply, State}
	end;
	
handle_info (
			{mosaic_component_process_internals, outbound_cast, Component, Request, RequestData},
			State = #state{status = Status})
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (RequestData) ->
	ok = mosaic_transcript:trace_error ("received unexpected outbound cast request; ignoring!", [{component, Component}, {request, Request}, {request_data, RequestData}, {status, Status}]),
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, inbound_register, Correlation, Group},
			State = #state{status = Status, harness = Harness})
		when is_binary (Correlation), (bit_size (Correlation) =:= 128), is_binary (Group), (bit_size (Group) =:= 160),
				((Status =:= executing) orelse (Status =:= migrating_as_target)) ->
	case mosaic_process_router:register (Group, erlang:self ()) of
		ok ->
			case mosaic_harness_frontend:push_packet (Harness, {exchange, [{<<"action">>, <<"register-return">>}, {<<"correlation">>, enforce_ok_1 (mosaic_component_coders:encode_correlation (Correlation))}, {<<"ok">>, true}], <<>>}) of
				ok ->
					{noreply, State};
				_Error = {error, _Reason} ->
					% !!!!
					{noreply, State}
			end;
		_Error = {error, Reason} ->
			case mosaic_harness_frontend:push_packet (Harness, {exchange, [{<<"action">>, <<"register-return">>}, {<<"correlation">>, enforce_ok_1 (mosaic_component_coders:encode_correlation (Correlation))}, {<<"ok">>, false}, {<<"error">>, enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason))}], <<>>}) of
				ok ->
					{noreply, State};
				_Error = {error, _Reason} ->
					% !!!!
					{noreply, State}
			end
	end;
	
handle_info (Message, State) ->
	ok = mosaic_transcript:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{noreply, State}.


begin_migration (source, Configuration, CompletionFun, OldState = #state{status = executing, execute = ExecuteSpecification}) ->
	case Configuration of
		defaults ->
			NewState = OldState#state{status = migrating_as_source},
			ok = CompletionFun ({prepared, [{execute, ExecuteSpecification}]}),
			ok = CompletionFun (completed),
			{continue, NewState};
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
		ok = case mosaic_harness_coders:validate_execute_specification (ExecuteSpecification) of
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
			{reject, Reason, OldState#state{status = migration_failed}}
	end.


commit_migration (OldState = #state{status = migrating_as_source, harness = Harness}) ->
	case mosaic_harness_frontend:stop (Harness, normal) of
		ok ->
			{continue, OldState#state{status = migrating_as_source_succeeded}};
		{error, Reason} ->
			{terminate, Reason, OldState#state{status = migrating_as_source_failed}}
	end;
	
commit_migration (OldState = #state{status = migrating_as_target, harness = Harness, execute = ExecuteSpecification}) ->
	case mosaic_harness_frontend:execute (Harness, ExecuteSpecification) of
		ok ->
			{continue, OldState#state{status = executing}};
		{error, Reason} ->
			{terminate, Reason, OldState#state{status = migrating_as_target_failed}}
	end.


rollback_migration (OldState = #state{status = migrating_as_source}) ->
	{continue, OldState#state{status = executing}};
	
rollback_migration (OldState = #state{status = Status}) when (Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_target) ->
	{continue, OldState#state{status = migrating_as_target_failed}}.


validate_configuration (
			Disposition,
			#configuration{harness = HarnessOptions, execute = ExecuteSpecification, router = Router})
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	try
		ok = if
			is_list (HarnessOptions) ->
				ok;
			true ->
				throw ({error, {invalid_harness, HarnessOptions}})
		end,
		ok = if
			(Disposition =:= create) ->
				ok = case mosaic_harness_coders:validate_execute_specification (ExecuteSpecification) of
					ok ->
						ok;
					Error1 = {error, _Reason1} ->
						throw (Error1)
				end;
			(Disposition =:= migrate), (ExecuteSpecification =:= migrate) ->
				ok;
			true ->
				throw ({error, {invalid_execute, ExecuteSpecification}})
		end,
		ok = if
			(is_pid (Router) orelse is_atom (Router)) ->
				ok;
			true ->
				throw ({error, {invalid_router, Router}})
		end,
		ok
	catch
		throw : Error = {error, _Reason} ->
			Error
	end;
	
validate_configuration (Disposition, Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	{error, {invalid_configuration, Configuration}};
	
validate_configuration (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


parse_configuration (Disposition, term, OriginalOptions)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)), is_list (OriginalOptions) ->
	DefaultOptions = [{harness, defaults}, {execute, undefined}, {router, defaults}],
	FinalOptions = OriginalOptions ++ DefaultOptions,
	case lists:sort (proplists:get_keys (FinalOptions)) of
		[execute, harness, router] ->
			try
				{ok, HarnessOptions} = case proplists:get_value (harness, FinalOptions) of
					undefined ->
						throw ({error, missing_harness});
					defaults ->
						{ok, []};
					HarnessOptions_ when is_list (HarnessOptions_) ->
						{ok, HarnessOptions_};
					HarnessOptions_ ->
						throw ({error, {invalid_harness, HarnessOptions_}})
				end,
				{ok, ExecuteSpecification} = case proplists:get_value (execute, FinalOptions) of
					undefined when (Disposition =:= create) ->
						throw ({error, missing_execute});
					undefined when (Disposition =:= migrate) ->
						{ok, migrate};
					ExecuteSpecification__ when (Disposition =:= create) ->
						case mosaic_harness_coders:decode_execute_specification (term, ExecuteSpecification__) of
							{ok, ExecuteSpecification_} ->
								{ok, ExecuteSpecification_};
							{error, Reason1} ->
								throw ({error, {invalid_execute, Reason1}})
						end;
					ExecuteSpecification__ when (Disposition =:= migrate) ->
						throw ({error, {unexpected_execute, ExecuteSpecification__}})
				end,
				{ok, Router} = case proplists:get_value (router, FinalOptions) of
					Router_ when (is_pid (Router_) orelse is_atom (Router_)) ->
						{ok, Router_};
					defaults ->
						{ok, mosaic_process_router};
					undefined ->
						throw ({error, missing_router});
					Router_ ->
						throw ({error, {invalid_router, Router_}})
				end,
				Configuration = #configuration{
						harness = HarnessOptions,
						execute = ExecuteSpecification,
						router = Router},
				{ok, Configuration}
			catch
				throw : Error = {error, _Reason} ->
					Error
			end;
		_ ->
			{error, {invalid_configuration, OriginalOptions}}
	end;
	
parse_configuration (Disposition, term, Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	case validate_configuration (Disposition, Configuration) of
		ok ->
			{ok, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_configuration (Disposition, Encoding, _Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	{error, {invalid_encoding, Encoding}};
	
parse_configuration (Disposition, _Encoding, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.
