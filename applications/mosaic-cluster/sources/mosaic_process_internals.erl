
-module (mosaic_process_internals).

-behaviour (gen_server).

-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-record (state, {qualified_name, identifier, status, callback_module, callback_state, migration_state}).
-record (migration_state, {role, status, token, migrator}).


init ({QualifiedName, {CallbackModule, Disposition, Identifier, Configuration}})
		when is_atom (CallbackModule), is_binary (Identifier), (bit_size (Identifier) =:= 160),
			((Disposition =:= create)
				orelse (is_record (Disposition, migrate, 2) andalso is_reference (element (2, Disposition)))) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			case Disposition of
				create ->
					case erlang:apply (CallbackModule, init, [create, Identifier, Configuration]) of
						{ok, CallbackState} ->
							{ok, #state{
									qualified_name = QualifiedName, identifier = Identifier, status = active,
									callback_module = CallbackModule, callback_state = CallbackState,
									migration_state = none}};
						Outcome = {stop, _Reason} ->
							Outcome
					end;
				{migrate, Token} ->
					case erlang:apply (CallbackModule, init, [migrate, Identifier, Configuration]) of
						{ok, CallbackState} ->
							{ok, #state{
									qualified_name = QualifiedName, identifier = Identifier, status = migrating,
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
	{stop, {invalid_arguments, Arguments}}.


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
		{reply, Outcome = ok, NewCallbackState} ->
			{reply, Outcome, OldState#state{callback_state = NewCallbackState}};
		{reply, Outcome = {ok, _Reply}, NewCallbackState} ->
			{reply, Outcome, OldState#state{callback_state = NewCallbackState}};
		{reply, Error = {error, _Reason}, NewCallbackState} ->
			{reply, Error, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Outcome = ok, NewCallbackState} ->
			{stop, Reason, Outcome, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Outcome = {ok, _Reply}, NewCallbackState} ->
			{stop, Reason, Outcome, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Error = {error, _Reason}, NewCallbackState} ->
			{stop, Reason, Error, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_call ({call, Request, RequestData}, Sender, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState})
		when is_binary (RequestData) ->
	case erlang:apply (CallbackModule, handle_call, [Request, RequestData, Sender, OldCallbackState]) of
		{reply, Outcome = {ok, _Reply, ReplyData}, NewCallbackState} when is_binary (ReplyData) ->
			{reply, Outcome, OldState#state{callback_state = NewCallbackState}};
		{reply, Error = {error, _Reason}, NewCallbackState} ->
			{reply, Error, NewCallbackState};
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Outcome = {ok, _Reply, ReplyData}, NewCallbackState} when is_binary (ReplyData) ->
			{stop, Reason, Outcome, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, Error = {error, _Reason}, NewCallbackState} ->
			{stop, Reason, Error, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_call ({begin_migration, Token, Arguments, Migrator}, _Sender, OldState)
		when is_pid (Migrator) ->
	case handle_begin_migration (Token, Arguments, Migrator, OldState) of
		{continue, Outcome = ok, NewState} ->
			{reply, Outcome, NewState};
		{continue, Error = {error, _Reason}, NewState} ->
			{reply, Error, NewState};
		{terminate, Reason, Outcome = ok, NewState} ->
			{stop, Reason, Outcome, NewState};
		{terminate, Reason, Error = {error, _Reason}, NewState} ->
			{stop, Reason, Error, NewState}
	end;
	
handle_call ({commit_migration, Token}, _Sender, OldState) ->
	case handle_commit_migration (Token, OldState) of
		{continue, Outcome = ok, NewState} ->
			{reply, Outcome, NewState};
		{continue, Error = {error, _Reason}, NewState} ->
			{reply, Error, NewState};
		{terminate, Reason, Outcome = ok, NewState} ->
			{stop, Reason, Outcome, NewState};
		{terminate, Reason, Error = {error, _Reason}, NewState} ->
			{stop, Reason, Error, NewState}
	end;
	
handle_call ({rollback_migration, Token}, _Sender, OldState) ->
	case handle_rollback_migration (Token, OldState) of
		{continue, Outcome = ok, NewState} ->
			{reply, Outcome, NewState};
		{continue, Error = {error, _Reason}, NewState} ->
			{reply, Error, NewState};
		{terminate, Reason, Outcome = ok, NewState} ->
			{stop, Reason, Outcome, NewState};
		{terminate, Reason, Error = {error, _Reason}, NewState} ->
			{stop, Reason, Error, NewState}
	end;
	
handle_call (Request, Sender, State) ->
	ok = mosaic_tools:trace_error ("received invalid call request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast ({cast, Request, RequestData}, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState})
		when is_binary (RequestData) ->
	case erlang:apply (CallbackModule, handle_cast, [Request, RequestData, OldCallbackState]) of
		{noreply, NewCallbackState} ->
			{noreply, OldState#state{callback_state = NewCallbackState}};
		{stop, Reason, NewCallbackState} ->
			{stop, Reason, OldState#state{callback_state = NewCallbackState}}
	end;
	
handle_cast (Request, State) ->
	ok = mosaic_tools:trace_error ("received invalid cast request; ignoring!", [{request, Request}]),
	{noreply, State}.


handle_info ({continue_migration, Token, Completion}, State) ->
	case handle_continue_migration (Token, Completion, State) of
		{continue, undefined, NewState} ->
			{noreply, NewState};
		{terminate, Reason, undefined, NewState} ->
			{stop, Reason, NewState}
	end;
	
handle_info (Message, OldState = #state{callback_module = CallbackModule, callback_state = OldCallbackState}) ->
	case erlang:apply (CallbackModule, handle_info, [Message, OldCallbackState]) of
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
	CompletionFunction = fun (Completion) -> Self ! {continue_migration, Token, Completion}, ok end,
	case erlang:apply (CallbackModule, begin_migration, [Role, Arguments, CompletionFunction, OldCallbackState]) of
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
	% !!!!
	{continue, {error, invalid_token}, State};
	
handle_begin_migration (_Token, _Arguments, _Migrator, State) ->
	% !!!!
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
	% !!!!
	{continue, undefined, State};
	
handle_continue_migration (
			Token, _Outcome,
			State = #state{status = migrating, migration_state = #migration_state{token = Token}}) ->
	% !!!!
	{continue, undefined, State};
	
handle_continue_migration (_Token, _Outcome, State) ->
	% !!!!
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
	% !!!!
	{continue, {error, invalid_token}, State};
	
handle_commit_migration (_Token, State) ->
	% !!!!
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
	% !!!!
	{continue, {error, invalid_token}, State};
	
handle_rollback_migration (_Token, State) ->
	% !!!!
	{continue, {error, invalid_state}, State}.
