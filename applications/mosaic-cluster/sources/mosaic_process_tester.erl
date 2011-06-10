
-module (mosaic_process_tester).


-behaviour (mosaic_process).


-export ([
		init/3, terminate/2, handle_stop/2, handle_call/5, handle_cast/4, handle_info/2,
		begin_migration/4, commit_migration/1, rollback_migration/1]).


-record (state, {
			identifier, status,
			terminate_delegate, handle_stop_delegate, handle_call_delegate, handle_cast_delegate, handle_info_delegate,
			begin_migration_delegate, commit_migration_delegate, rollback_migration_delegate,
			migration_outcome}).


init (create, Identifier, defaults)
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	init (create, Identifier, ok);
	
init (create, Identifier, ok)
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	{ok, #state{
			identifier = Identifier, status = active,
			terminate_delegate = none,
			handle_stop_delegate = none, handle_call_delegate = none, handle_cast_delegate = none, handle_info_delegate = none,
			begin_migration_delegate = none, commit_migration_delegate = none, rollback_migration_delegate = none,
			migration_outcome = none}};
	
init (create, Identifier, {stop, Reason})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	{stop, Reason};
	
init (migrate, Identifier, defaults)
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	init (migrate, Identifier, ok);
	
init (migrate, Identifier, ok)
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	{ok, #state{
			status = migrating_as_target_waiting_begin,
			terminate_delegate = none,
			handle_stop_delegate = none, handle_call_delegate = none, handle_cast_delegate = none, handle_info_delegate = none,
			begin_migration_delegate = none, commit_migration_delegate = none, rollback_migration_delegate = none,
			migration_outcome = none}};
	
init (migrate, Identifier, {stop, Reason})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	{stop, Reason}.


terminate (Reason, State = #state{terminate_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (Reason, State);
	
terminate (_Reason, #state{status = Status})
		when ((Status =:= active) orelse (Status =:= stopped)
				orelse (Status =:= migrating_as_source_succeeded) orelse (Status =:= migrating_as_source_failed)
				orelse (Status =:= migrating_as_target_waiting_begin) orelse (Status =:= migrating_as_target_failed)) ->
	ok.


handle_stop (Signal, State = #state{handle_stop_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (Signal, State);
	
handle_stop (normal, State = #state{status = active}) ->
	{stop, normal, ok, State#state{status = stopped}};
	
handle_stop ({reply, Reply}, State = #state{status = active}) ->
	{reply, Reply, State};
	
handle_stop ({stop, Reason, Reply}, State = #state{status = active}) ->
	{stop, Reason, Reply, State};
	
handle_stop ({delegate, Delegate}, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_stop (Signal, State = #state{status = active}) ->
	{reply, {error, {invalid_signal, Signal}}, State};
	
handle_stop (_Signal, State = #state{status = Status}) ->
	{reply, {error, {invalid_status, Status}}, State}.


handle_call (Operation, Inputs, Data, Sender, State = #state{handle_call_delegate = Delegate})
		when is_binary (Operation), is_binary (Data), (Delegate =/= none) ->
	Delegate (Operation, Inputs, Data, Sender, State);
	
handle_call (<<"status">>, null, <<>>, _Sender, State = #state{status = Status}) ->
	{reply, {ok, Status, <<>>}, State};
	
handle_call (<<"set_terminate_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 2)) ->
	{reply, {ok, null, <<>>}, State#state{terminate_delegate = Delegate}};
	
handle_call (<<"set_handle_stop_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 2)) ->
	{reply, {ok, null, <<>>}, State#state{handle_stop_delegate = Delegate}};
	
handle_call (<<"set_handle_call_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 5)) ->
	{reply, {ok, null, <<>>}, State#state{handle_call_delegate = Delegate}};
	
handle_call (<<"set_handle_cast_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 4)) ->
	{reply, {ok, null, <<>>}, State#state{handle_cast_delegate = Delegate}};
	
handle_call (<<"set_handle_info_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 2)) ->
	{reply, {ok, null, <<>>}, State#state{handle_info_delegate = Delegate}};
	
handle_call (<<"set_begin_migration_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 3)) ->
	{reply, {ok, null, <<>>}, State#state{begin_migration_delegate = Delegate}};
	
handle_call (<<"set_commit_migration_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 1)) ->
	{reply, {ok, null, <<>>}, State#state{commit_migration_delegate = Delegate}};
	
handle_call (<<"set_rollback_migration_delegate">>, Delegate, <<>>, _Sender, State)
		when ((Delegate =:= none) orelse is_function (Delegate, 1)) ->
	{reply, {ok, null, <<>>}, State#state{rollback_migration_delegate = Delegate}};
	
handle_call (<<"ping">>, {Target, Token1, Return}, <<>>, Sender, State)
		when is_pid (Target), ((Return =:= reply) orelse (Return =:= noreply)) ->
	Token2 = erlang:make_ref (),
	Target ! {pong, call, Token1, Token2},
	case Return of
		reply ->
			{reply, {ok, {pong, Token1, Token2}, <<>>}, State};
		noreply ->
			_ = gen_server:reply (Sender, {ok, {pong, Token1, Token2}, <<>>}),
			{noreply, State}
	end;
	
handle_call (<<"reply">>, Reply, <<>>, _Sender, State) ->
	{reply, Reply, State};
	
handle_call (<<"stop">>, {Reason, Reply}, <<>>, _Sender, State) ->
	{stop, Reason, Reply, State};
	
handle_call (<<"delegate">>, Delegate, <<>>, _Sender, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_call (Operation, Inputs, Data, _Sender, State)
		when is_binary (Operation), is_binary (Data) ->
	Error = {error, {invalid_request, {Operation, Inputs, Data}}},
	{stop, Error, Error, State}.


handle_cast (Operation, Inputs, Data, State = #state{handle_cast_delegate = Delegate})
		when is_binary (Operation), is_binary (Data), (Delegate =/= none) ->
	Delegate (Operation, Inputs, Data, State);
	
handle_cast (<<"ping">>, {Target, Token}, <<>>, State)
		when is_pid (Target) ->
	Target ! {pong, cast, Token, Token},
	{noreply, State};
	
handle_cast (<<"stop">>, Reason, <<>>, State) ->
	{stop, Reason, State};
	
handle_cast (<<"delegate">>, Delegate, <<>>, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_cast (Operation, Inputs, Data, State)
		when is_binary (Operation), is_binary (Data) ->
	{stop, {error, {invalid_request, {Operation, Inputs, Data}}}, State}.


handle_info (Message, State = #state{handle_info_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (Message, State);
	
handle_info ({ping, Target, Token}, State)
		when is_pid (Target) ->
	Target ! {pong, info, Token, Token},
	{noreply, State};
	
handle_info ({stop, Reason}, State) ->
	{stop, Reason, State};
	
handle_info ({delegate, Delegate}, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_info (Message, State) ->
	{stop, {invalid_message, Message}, State}.


begin_migration (Disposition, Configuration, CompletionFunction, State = #state{begin_migration_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (Disposition, Configuration, CompletionFunction, State);
	
begin_migration (source, defaults, CompletionFunction, State) ->
	begin_migration (source, {continue, {continue, succeed}, succeed}, CompletionFunction, State);
	
begin_migration (source, {continue, Configuration, Outcome}, CompletionFunction, State = #state{status = active})
		when is_function (CompletionFunction, 1), ((Outcome =:= succeed) orelse (Outcome =:= fail)) ->
	ok = CompletionFunction ({prepared, Configuration}),
	ok = CompletionFunction (completed),
	{continue, State#state{status = migrating_as_source_waiting_commit, migration_outcome = Outcome}};
	
begin_migration (target, {continue, Outcome}, CompletionFunction, State = #state{status = migrating_as_target_waiting_begin})
		when is_function (CompletionFunction, 1), ((Outcome =:= succeed) orelse (Outcome =:= fail)) ->
	ok = CompletionFunction (completed),
	{continue, State#state{status = migrating_as_target_waiting_commit, migration_outcome = Outcome}};
	
begin_migration (Role, Continuation, CompletionFunction, State = #state{status = Status})
		when (((Role =:= source) andalso (Status =:= active))
					orelse ((Role =:= target) andalso (Status =:= migrating_as_target_waiting_begin))),
				((Continuation =:= reject) orelse (Continuation =:= terminate)),
				is_function (CompletionFunction, 1) ->
	case Continuation of
		reject ->
			{reject, rejected, State};
		terminate ->
			{terminate, terminated,
					State#state{
							status = if
								(Role =:= source) -> migrating_as_source_failed;
								(Role =:= target) -> migrating_as_target_failed
							end}}
	end;
	
begin_migration (Role, Configuration, CompletionFunction, State = #state{status = Status})
		when ((Role =:= source) orelse (Role =:= target)), ((Status =:= active) orelse (Status =:= migrating_as_target_waiting_begin)),
				is_function (CompletionFunction, 1) ->
	{reject, {invalid_configuration, Configuration}, State};
	
begin_migration (Role, _Configuration, CompletionFunction, State = #state{status = Status})
		when ((Role =:= source) orelse (Role =:= target)), is_function (CompletionFunction, 1) ->
	{reject, {invalid_status, Status}, State}.


commit_migration (State = #state{commit_migration_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (State);
	
commit_migration (State = #state{status = migrating_as_source_waiting_commit, migration_outcome = Outcome})
		when ((Outcome =:= succeed) orelse (Outcome =:= fail)) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = migrating_as_source_succeeded}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_source_failed}}
	end;
	
commit_migration (State = #state{status = migrating_as_target_waiting_commit, migration_outcome = Outcome})
		when ((Outcome =:= succeed) orelse (Outcome =:= fail)) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = active}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_target_failed}}
	end.


rollback_migration (State = #state{rollback_migration_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (State);
	
rollback_migration (State = #state{status = migrating_as_source_waiting_commit, migration_outcome = Outcome})
		when ((Outcome =:= succeed) orelse (Outcome =:= fail)) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = active}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_source_failed}}
	end;
	
rollback_migration (State = #state{status = migrating_as_target_waiting_commit, migration_outcome = Outcome})
		when ((Outcome =:= succeed) orelse (Outcome =:= fail)) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = migrating_as_target_failed}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_target_failed}}
	end.
