
-module (mosaic_component_process).

-behaviour (mosaic_process).


-export ([validate_configuration/2, parse_configuration/3]).
-export ([
		init/3, terminate/2, handle_stop/2,
		handle_call/4, handle_cast/3, handle_info/2,
		begin_migration/4, commit_migration/1, rollback_migration/1]).


-record (state, {identifier, status, harness, harness_token, execute, router, inbound_pending_calls, outbound_pending_calls}).
-record (configuration, {harness, execute, router}).
-record (inbound_pending_call, {correlation, sender}).
-record (outbound_pending_call, {reference, correlation}).


init (Disposition, Identifier, OriginalConfiguration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate))->
	try
		case parse_configuration (Disposition, term, OriginalConfiguration) of
			{ok, #configuration{harness = HarnessConfiguration, execute = ExecuteSpecification, router = Router}} ->
				Status = if (Disposition =:= create) -> executing; (Disposition =:= migrate) -> pre_migrating_as_target end,
				init (prepare, Identifier, {Status, HarnessConfiguration, ExecuteSpecification, Router});
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end
	catch
		throw : {error, Reason} ->
			{stop, Reason}
	end;
	
init (prepare, Identifier, {Status, HarnessConfiguration, ExecuteSpecification, Router}) ->
	true = erlang:process_flag (trap_exit, true),
	try
		HarnessToken = erlang:make_ref (),
		{ok, Harness} = case mosaic_component_harness:start_link ([{controller, erlang:self ()}, {controller_token, HarnessToken} | HarnessConfiguration]) of
			Outcome = {ok, _Harness} ->
				Outcome;
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end,
		ok = if
			(ExecuteSpecification =/= migrate) ->
				ok = case mosaic_component_harness:execute (Harness, ExecuteSpecification) of
					ok ->
						ok;
					Error2 = {error, _Reason2} ->
						throw (Error2)
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


terminate (_Reason, _State = #state{harness = Harness}) ->
	_ = mosaic_component_harness:stop (Harness),
	_ = mosaic_tools:wait (Harness, 5000),
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
			{mosaic_component_harness, exchange, HarnessToken, {MetaData, Data}},
			State = #state{status = Status, harness_token = HarnessToken})
		when is_list (MetaData), is_binary (Data),
				((Status =:= executing) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	case lists:sort (MetaData) of
		[{<<"action">>, <<"return">>}, {<<"correlation">>, Correlation_}, {<<"meta-data">>, Reply}]
				when is_binary (Correlation_) ->
			case mosaic_webmachine:parse_string_identifier (Correlation_) of
				{ok, Correlation} ->
					handle_info ({mosaic_component_process_internals, inbound_return, Correlation, {ok, Reply, Data}}, State);
				{error, Reason} ->
					ok = mosaic_tools:trace_error ("received invalid inbound call return: invalid correlation identifier; ignoring!", [{correlation, Correlation_}, {reason, Reason}]),
					{noreply, State}
			end;
		[{<<"action">>, <<"call">>}, {<<"component">>, Component_}, {<<"correlation">>, Correlation_}, {<<"meta-data">>, Request}]
				when is_binary (Component_), is_binary (Correlation_) ->
			case mosaic_webmachine:parse_string_identifier (Component_) of
				{ok, Component} ->
					case mosaic_webmachine:parse_string_identifier (Correlation_) of
						{ok, Correlation} ->
							handle_info ({mosaic_component_process_internals, outbound_call, Component, Correlation, Request, Data}, State);
						{error, Reason} ->
							ok = mosaic_tools:trace_error ("received invalid outbound call request: invalid correlation identifier; ignoring!", [{correlation, Correlation_}, {reason, Reason}]),
							{noreply, State}
					end;
				{error, Reason} ->
					ok = mosaic_tools:trace_error ("received invalid outbound call request: invalid component identifier; ignoring!", [{component, Component_}, {reason, Reason}]),
					{noreply, State}
			end;
		[{<<"action">>, <<"cast">>}, {<<"component">>, Component_}, {<<"meta-data">>, Request}]
				when is_binary (Component_) ->
			case mosaic_webmachine:parse_string_identifier (Component_) of
				{ok, Component} ->
					handle_info ({mosaic_component_process_internals, outbound_cast, Component, Request, Data}, State);
				{error, Reason} ->
					ok = mosaic_tools:trace_error ("received invalid outbound cast request: invalid component identifier; ignoring!", [{component, Component_}, {reason, Reason}]),
					{noreply, State}
			end;
		_ ->
			ok = mosaic_tools:trace_error ("received invalid exchange meta-data; ignoring!", [{meta_data, MetaData}, {data, Data}]),
			{noreply, State}
	end;
	
handle_info (
			{mosaic_component_harness, exit, HarnessToken, ExitCode},
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
	{ok, Correlation} = mosaic_cluster_tools:key (),
	MetaData = [{<<"action">>, <<"call">>}, {<<"correlation">>, mosaic_webmachine:format_string_identifier (Correlation)}, {<<"meta-data">>, Request}],
	PendingCall = #inbound_pending_call{correlation = Correlation, sender = Sender},
	NewPendingCalls = orddict:store (Correlation, PendingCall, OldPendingCalls),
	NewState = OldState#state{inbound_pending_calls = NewPendingCalls},
	case mosaic_component_harness:exchange (Harness, {MetaData, RequestData}) of
		ok ->
			{noreply, NewState};
		Error = {error, _Reason} ->
			handle_info ({mosaic_component_process_internals, inbound_return, Correlation, Error}, NewState)
	end;
	
handle_info (
			{mosaic_component_process_internals, inbound_call, Request, RequestData, Sender},
			State = #state{status = Status})
		when is_binary (RequestData) ->
	ok = mosaic_tools:trace_error ("received unexpected inbound call; ignoring!", [{request, Request}, {request_data, RequestData}, {sender, Sender}]),
	_ = gen_server:reply (Sender, {error, {invalid_status, Status}}),
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, inbound_return, Correlation, Outcome},
			OldState = #state{status = Status, inbound_pending_calls = OldPendingCalls})
		when is_binary (Correlation), (bit_size (Correlation) =:= 160),
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
					ok = mosaic_tools:trace_error ("received invalid inbound return: invalid outcome; ignoring!", [{correlation, Correlation}, {outcome, Outcome}]),
					ok
			end,
			NewPendingCalls = orddict:erase (Correlation, OldPendingCalls),
			NewState = OldState#state{inbound_pending_calls = NewPendingCalls},
			{noreply, NewState};
		error ->
			ok = mosaic_tools:trace_error ("received invalid inbound return: invalid correlation; ignoring!", [{correlation, Correlation}, {outcome, Outcome}]),
			{noreply, OldState}
	end;
	
handle_info (
			{mosaic_component_process_internals, inbound_return, Correlation, _Outcome},
			State = #state{status = _Status})
		when is_binary (Correlation), (bit_size (Correlation) =:= 160) ->
	% !!!!
	{noreply, State};
	
handle_info (
			{mosaic_component_process_internals, outbound_call, Component, Correlation, Request, RequestData},
			OldState = #state{status = Status, router = Router, outbound_pending_calls = OldPendingCalls})
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (Correlation), (bit_size (Correlation) =:= 160), is_binary (RequestData),
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
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (Correlation), (bit_size (Correlation) =:= 160), is_binary (RequestData) ->
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
					MetaData = [{<<"action">>, <<"return">>}, {<<"correlation">>, mosaic_webmachine:format_string_identifier (Correlation)}, {<<"meta-data">>, Reply}],
					case mosaic_component_harness:exchange (Harness, {MetaData, ReplyData}) of
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
	case mosaic_component_harness:exchange (Harness, {MetaData, RequestData}) of
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
	ok = mosaic_tools:trace_error ("received unexpected outbound cast request; ignoring!", [{component, Component}, {request, Request}, {request_data, RequestData}, {status, Status}]),
	{noreply, State};
	
handle_info (Message, State) ->
	ok = mosaic_tools:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{noreply, State}.


begin_migration (source, OriginalConfiguration, CompletionFun, OldState = #state{status = executing, execute = ExecuteSpecification}) ->
	case OriginalConfiguration of
		defaults ->
			NewState = OldState#state{status = migrating_as_source},
			Configuration = [{execute, ExecuteSpecification}],
			ok = CompletionFun ({prepared, Configuration}),
			ok = CompletionFun (completed),
			{continue, NewState};
		_ ->
			{reject, {invalid_configuration, OriginalConfiguration}, OldState}
	end;
	
begin_migration (target, OriginalConfiguration, CompletionFun, OldState = #state{status = pre_migrating_as_target}) ->
	try
		{ok, OriginalExecuteSpecification} = case OriginalConfiguration of
			[{execute, OriginalExecuteSpecification_}] ->
				{ok, OriginalExecuteSpecification_};
			_ ->
				throw ({invalid_configuration, OriginalConfiguration})
		end,
		{ok, ExecuteSpecification} = case mosaic_component_harness:parse_execute_specification (OriginalExecuteSpecification) of
			{ok, ExecuteSpecification_} ->
				{ok, ExecuteSpecification_};
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
	case mosaic_component_harness:stop (Harness, normal) of
		ok ->
			{continue, OldState#state{status = migrating_as_source_succeeded}};
		{error, Reason} ->
			{terminate, Reason, OldState#state{status = migrating_as_source_failed}}
	end;
	
commit_migration (OldState = #state{status = migrating_as_target, harness = Harness, execute = ExecuteSpecification}) ->
	case mosaic_component_harness:execute (Harness, ExecuteSpecification) of
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
			#configuration{harness = HarnessConfiguration, execute = ExecuteSpecification, router = Router})
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	try
		ok = if
			is_list (HarnessConfiguration) ->
				ok;
			true ->
				throw ({error, {invalid_harness, HarnessConfiguration}})
		end,
		ok = if
			(Disposition =:= create) ->
				ok = case mosaic_component_harness:validate_execute_specification (ExecuteSpecification) of
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
				{ok, HarnessConfiguration} = case proplists:get_value (harness, FinalOptions) of
					undefined ->
						throw ({error, missing_harness});
					defaults ->
						{ok, []};
					HarnessConfiguration_ when is_list (HarnessConfiguration_) ->
						{ok, HarnessConfiguration_};
					HarnessConfiguration_ ->
						throw ({error, {invalid_harness, HarnessConfiguration_}})
				end,
				{ok, ExecuteSpecification} = case proplists:get_value (execute, FinalOptions) of
					undefined when (Disposition =:= create) ->
						throw ({error, missing_execute});
					undefined when (Disposition =:= migrate) ->
						{ok, migrate};
					ExecuteSpecification__ when (Disposition =:= create) ->
						case mosaic_component_harness:parse_execute_specification (ExecuteSpecification__) of
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
						harness = HarnessConfiguration,
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
