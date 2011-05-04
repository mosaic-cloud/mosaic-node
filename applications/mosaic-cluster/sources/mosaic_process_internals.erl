
-module (mosaic_process_internals).

-behaviour (gen_server).

-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-record (state, {qualified_name, status, callback_module, callback_state, migration_state}).
-record (migration_state, {role, status, token, migrator}).


init ({QualifiedName, {CallbackModule, Disposition}})
		when is_atom (CallbackModule) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			case Disposition of
				{create, Arguments} ->
					case erlang:apply (CallbackModule, init, [{create, Arguments}]) of
						{ok, CallbackState} ->
							{ok, #state{
									qualified_name = QualifiedName, status = active,
									callback_module = CallbackModule, callback_state = CallbackState,
									migration_state = none}};
						Outcome = {stop, _Reason} ->
							Outcome
					end;
				{migrate, Token} ->
					case erlang:apply (CallbackModule, init, [migrate]) of
						{ok, CallbackState} ->
							{ok, #state{
									qualified_name = QualifiedName, status = migrating,
									callback_module = CallbackModule, callback_state = CallbackState,
									migration_state = #migration_state{
											role = target, status = waiting_begin, token = Token, migrator = none}}};
						Outcome = {stop, _Reason} ->
							Outcome
					end
			end;
		{error, Reason} ->
			{stop, Reason}
	end;
	
init (Arguments) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, init, invalid_arguments, {Arguments}),
	{stop, invalid_arguments}.


terminate (
			Reason,
			_State = #state{
					status = Status, callback_module = CallbackModule, callback_state = CallbackState, migration_state = none})
		when (Status =:= active) or (Status =:= migration_succeeded) or (Status =:= migration_failed) ->
	ok = erlang:apply (CallbackModule, terminate, [Reason, CallbackState]);
	
terminate (
			Reason,
			OldState = #state{status = migrating, migration_state = #migration_state{token = Token}}) ->
	case handle_rollback_migration (Token, OldState) of
		{continue, _Reply, NewState} ->
			terminate (Reason, NewState);
		{terminate, NewReason, _Reply, NewState} ->
			terminate (NewReason, NewState)
	end.


code_change (_OldVsn, State, _Data) ->
	{ok, State}.


handle_call ({stop, Signal}, _Sender, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	case erlang:apply (CallbackModule, handle_stop, [Signal, OldCallbackState]) of
		{reply, Reply, NewCallbackState} ->
			{reply, Reply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Reply, NewCallbackState} ->
			{stop, Reason, Reply, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_call ({call, Request}, Sender, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	case erlang:apply (CallbackModule, handle_call, [Request, Sender, OldCallbackState]) of
		{reply, Reply, NewCallbackState} ->
			{reply, Reply, OldState#state{callback_state = NewCallbackState}};
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Reply, NewCallbackState} ->
			{stop, Reason, Reply, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_call ({begin_migration, Token, Arguments, Migrator}, _Sender, OldState)
		when is_pid (Migrator) ->
	case handle_begin_migration (Token, Arguments, Migrator, OldState) of
		{continue, Reply, NewState} ->
			{reply, Reply, NewState};
		{terminate, Reason, Reply, NewState} ->
			{stop, Reason, Reply, NewState}
	end;
	
handle_call ({commit_migration, Token}, _Sender, OldState) ->
	case handle_commit_migration (Token, OldState) of
		{continue, Reply, NewState} ->
			{reply, Reply, NewState};
		{terminate, Reason, Reply, NewState} ->
			{stop, Reason, Reply, NewState}
	end;
	
handle_call ({rollback_migration, Token}, _Sender, OldState) ->
	case handle_rollback_migration (Token, OldState) of
		{continue, Reply, NewState} ->
			{reply, Reply, NewState};
		{terminate, Reason, Reply, NewState} ->
			{stop, Reason, Reply, NewState}
	end;
	
handle_call (Request, _Sender, State) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_call, invalid_request, {Request}),
	{reply, {error, invalid_request}, State}.


handle_cast ({cast, Request}, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	case erlang:apply (CallbackModule, handle_cast, [Request, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_cast (Request, State) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_cast, invalid_request, {Request}),
	{noreply, State}.


handle_info ({continue_migration, Token, Completion}, State) ->
	case handle_continue_migration (Token, Completion, State) of
		{continue, undefined, NewState} ->
			{noreply, NewState};
		{terminate, Reason, undefined, NewState} ->
			{stop, Reason, NewState}
	end;
	
handle_info (Message, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	case erlang:apply (CallbackModule, handle_cast, [Message, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}}
	end.


handle_begin_migration (Token, Arguments, Migrator, OldState = #state{status = active, migration_state = none}) ->
	NewState = OldState#state{
			status = migrating,
			migration_state = #migration_state{role = source, status = waiting_begin, token = Token, migrator = none}},
	handle_begin_migration (Token, Arguments, Migrator, NewState);
	
handle_begin_migration (
			Token, Arguments, Migrator,
			OldState = #state{
					status = migrating, callback_module = CallbackModule, callback_state = OldCallbackState,
					migration_state = OldMigrationState = #migration_state{
							role = Role, status = waiting_begin, token = Token, migrator = none}})
		when (Role =:= source) or (Role =:= target) ->
	Self = erlang:self (),
	CompletionFun = fun (Completion) -> Self ! {continue_migration, Token, Completion}, ok end,
	case erlang:apply (CallbackModule, begin_migration, [{Role, Arguments, CompletionFun}, OldCallbackState]) of
		{continue, NewCallbackState} ->
			Migrator ! {begin_migration, Token, succeeded},
			NewMigrationStatus = case Role of source -> waiting_prepared; target -> waiting_completed end,
			NewState = OldState#state{
					callback_state = NewCallbackState,
					migration_state = OldMigrationState#migration_state{status = NewMigrationStatus, migrator = Migrator}},
			{continue, ok, NewState};
		{reject, Reason, NewCallbackState} ->
			Migrator ! {begin_migration, Token, failed, Reason},
			case Role of
				source ->
					NewState = OldState#state{status = active, callback_state = NewCallbackState, migration_state = none},
					{continue, {error, Reason}, NewState};
				target ->
					NewState = OldState#state{status = migration_failed, callback_state = NewCallbackState, migration_state = none},
					{terminate, {migration_failed, Reason}, {error, Reason}, NewState}
			end;
		{terminate, Reason, NewCallbackState} ->
			case Role of
				source ->
					NewState = OldState#state{status = active, callback_state = NewCallbackState, migration_state = none},
					{terminate, {migration_failed, Reason}, {error, Reason}, NewState};
				target ->
					NewState = OldState#state{status = migration_failed, callback_state = NewCallbackState, migration_state = none},
					{terminate, {migration_failed, Reason}, {error, Reason}, NewState}
			end
	end;
	
handle_begin_migration (
			Token, _Arguments, _Migrator,
			State = #state{status = migrating, migration_state = #migration_state{token = OtherToken}})
		when (Token =/= OtherToken) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_begin_migration, invalid_token, {Token}),
	{continue, {error, invalid_token}, State};
	
handle_begin_migration (_Token, _Arguments, _Migrator, State) ->
	{continue, {error, invalid_state}, State}.


handle_continue_migration (
			Token, {prepared, Arguments},
			OldState = #state{
					status = migrating,
					migration_state = OldMigrationState = #migration_state{
							role = source, status = waiting_prepared, token = Token, migrator = Migrator}}) ->
	Migrator ! {continue_migration, Token, prepared, Arguments},
	{continue, undefined, OldState#state{migration_state = OldMigrationState#migration_state{status = waiting_completed}}};
	
handle_continue_migration (
			Token, completed,
			OldState = #state{
					status = migrating,
					migration_state = OldMigrationState = #migration_state{
							role = Role, status = waiting_completed, token = Token, migrator = Migrator}})
		when (Role =:= source) or (Role =:= target) ->
	Migrator ! {continue_migration, Token, completed},
	{continue, undefined, OldState#state{migration_state = OldMigrationState#migration_state{status = waiting_commit}}};
	
handle_continue_migration (
			Token, _Outcome,
			State = #state{status = migrating, migration_state = #migration_state{token = OtherToken}})
		when (Token =/= OtherToken) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_continue_migration, invalid_token, {Token}),
	{continue, undefined, State};
	
handle_continue_migration (
			Token, Outcome,
			State = #state{status = migrating, migration_state = #migration_state{token = Token}}) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_continue_migration, invalid_outcome, {Outcome}),
	{continue, undefined, State};
	
handle_continue_migration (_Token, _Outcome, State) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_continue_migration, invalid_state, none),
	{continue, undefined, State}.


handle_commit_migration (
			Token,
			OldState = #state{
					status = migrating, callback_module = CallbackModule, callback_state = OldCallbackState,
					migration_state = #migration_state{
							role = Role, status = waiting_commit, token = Token, migrator = Migrator}})
		when (Role =:= source) or (Role =:= target) ->
	case erlang:apply (CallbackModule, commit_migration, [OldCallbackState]) of
		{continue, NewCallbackState} ->
			Migrator ! {commit_migration, Token, succeeded},
			case Role of
				source ->
					NewState = OldState#state{status = migration_succeeded, callback_state = NewCallbackState, migration_state = none},
					{terminate, normal, ok, NewState};
				target ->
					NewState = OldState#state{status = active, callback_state = NewCallbackState, migration_state = none},
					{continue, ok, NewState}
			end;
		{terminate, Reason, NewCallbackState} ->
			Migrator ! {commit_migration, Token, failed, Reason},
			NewState = OldState#state{status = migration_failed, callback_state = NewCallbackState, migration_state = none},
			{terminate, {migration_failed, Reason}, {error, Reason}, NewState}
	end;
	
handle_commit_migration (
			Token,
			State = #state{status = migrating, migration_state = #migration_state{token = OtherToken}})
		when (Token =/= OtherToken) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_commit_migration, invalid_token, {Token}),
	{continue, {error, invalid_token}, State};
	
handle_commit_migration (_Token, State) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_commit_migration, invalid_state, none),
	{continue, {error, invalid_state}, State}.


handle_rollback_migration (
			Token,
			OldState = #state{
					status = migrating, callback_module = CallbackModule, callback_state = OldCallbackState,
					migration_state = #migration_state{
							role = Role, status = _, token = Token, migrator = Migrator}})
		when (Role =:= source) or (Role =:= target) ->
	case erlang:apply (CallbackModule, rollback_migration, [OldCallbackState]) of
		{continue, NewCallbackState} ->
			Migrator ! {rollback_migration, Token, succeeded},
			case Role of
				source ->
					NewState = OldState#state{status = active, callback_state = NewCallbackState, migration_state = none},
					{continue, ok, NewState};
				target ->
					NewState = OldState#state{status = migration_failed, callback_state = NewCallbackState, migration_state = none},
					{terminate, normal, ok, NewState}
			end;
		{terminate, Reason, NewCallbackState} ->
			Migrator ! {rollback_migration, Token, failed, Reason},
			NewState = OldState#state{status = migration_failed, callback_state = NewCallbackState, migration_state = none},
			{terminate, {migration_failed, Reason}, {error, Reason}, NewState}
	end;
	
handle_rollback_migration (
			Token,
			State = #state{status = migrating, migration_state = #migration_state{token = OtherToken}})
		when (Token =/= OtherToken) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_rollback_migration, invalid_token, {Token}),
	{continue, {error, invalid_token}, State};
	
handle_rollback_migration (_Token, State) ->
	ok = mosaic_tools:report_error (mosaic_process_internals, handle_rollback_migration, invalid_state, none),
	{continue, {error, invalid_state}, State}.
