
-module (mosaic_process_tester).

-behaviour (mosaic_process).

-export ([
		init/1, terminate/2,
		handle_stop/2, handle_call/3, handle_cast/2, handle_info/2,
		begin_migration/2, commit_migration/1, rollback_migration/1]).


-record (state, {status, migration_outcome, terminate_delegate, begin_migration_delegate, commit_migration_delegate, rollback_migration_delegate}).


init ({create, ok}) ->
	{ok, #state{
			status = active, migration_outcome = undefined,
			terminate_delegate = none,
			begin_migration_delegate = none, commit_migration_delegate = none, rollback_migration_delegate = none}};
	
init ({create, {stop, Reason}}) ->
	{stop, Reason};
	
init (migrate) ->
	{ok, #state{
			status = migrating_as_target_waiting_begin,
			terminate_delegate = none,
			begin_migration_delegate = none, commit_migration_delegate = none, rollback_migration_delegate = none}}.


terminate (Reason, State = #state{terminate_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (Reason, State);
	
terminate (_Reason, #state{status = Status})
		when (Status =:= active) or (Status =:= stopped)
				or (Status =:= migrating_as_source_succeeded) or (Status =:= migrating_as_source_failed)
				or (Status =:= migrating_as_target_waiting_begin) or (Status =:= migrating_as_target_failed) ->
	ok.


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


handle_call (status, _Sender, State = #state{status = Status}) ->
	{reply, {ok, Status}, State};
	
handle_call ({terminate_delegate, Delegate}, _Sender, State)
		when (Delegate =:= none) or is_function (Delegate, 2) ->
	{reply, ok, State#state{terminate_delegate = Delegate}};
	
handle_call ({begin_migration_delegate, Delegate}, _Sender, State)
		when (Delegate =:= none) or is_function (Delegate, 2) ->
	{reply, ok, State#state{begin_migration_delegate = Delegate}};
	
handle_call ({commit_migration_delegate, Delegate}, _Sender, State)
		when (Delegate =:= none) or is_function (Delegate, 1) ->
	{reply, ok, State#state{commit_migration_delegate = Delegate}};
	
handle_call ({rollback_migration_delegate, Delegate}, _Sender, State)
		when (Delegate =:= none) or is_function (Delegate, 1) ->
	{reply, ok, State#state{rollback_migration_delegate = Delegate}};
	
handle_call ({ping, Target, Token1, Return}, Sender, State)
		when is_pid (Target), (Return =:= reply) or (Return =:= noreply) ->
	Token2 = erlang:make_ref (),
	Target ! {pong, Token1, Token2},
	case Return of
		reply ->
			{reply, {pong, Token1, Token2}, State};
		noreply ->
			_ = gen_server:reply (Sender, {pong, Token1, Token2}),
			{noreply, State}
	end;
	
handle_call ({reply, Reply}, _Sender, State) ->
	{reply, Reply, State};
	
handle_call ({stop, Reason, Reply}, _Sender, State) ->
	{stop, Reason, Reply, State};
	
handle_call ({delegate, Delegate}, _Sender, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_call (Request, _Sender, State) ->
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast ({ping, Target, Token}, State)
		when is_pid (Target) ->
	Target ! {pong, Token, Token},
	{noreply, State};
	
handle_cast ({stop, Reason}, State) ->
	{stop, Reason, State};
	
handle_cast ({delegate, Delegate}, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_cast (_Request, State) ->
	{noreply, State}.


handle_info ({ping, Target, Token}, State)
		when is_pid (Target) ->
	Target ! {pong, Token, Token},
	{noreply, State};
	
handle_info ({stop, Reason}, State) ->
	{stop, Reason, State};
	
handle_info ({delegate, Delegate}, State)
		when is_function (Delegate, 1) ->
	Delegate (State);
	
handle_info (_Message, State) ->
	{noreply, State}.


begin_migration (Disposition, State = #state{begin_migration_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (Disposition, State);
	
begin_migration ({source, defaults, CompletionFun}, State) ->
	begin_migration ({source, {continue, {continue, succeed}, succeed}, CompletionFun}, State);
	
begin_migration ({source, {continue, Arguments, Outcome}, CompletionFun}, State = #state{status = active})
		when is_function (CompletionFun, 1), (Outcome =:= succeed) or (Outcome =:= fail) ->
	ok = CompletionFun ({prepared, Arguments}),
	ok = CompletionFun (completed),
	{continue, State#state{status = migrating_as_source_waiting_commit, migration_outcome = Outcome}};
	
begin_migration ({target, {continue, Outcome}, CompletionFun}, State = #state{status = migrating_as_target_waiting_begin})
		when is_function (CompletionFun, 1), (Outcome =:= succeed) or (Outcome =:= fail) ->
	ok = CompletionFun (completed),
	{continue, State#state{status = migrating_as_target_waiting_commit, migration_outcome = Outcome}};
	
begin_migration ({Role, Continuation, CompletionFun}, State = #state{status = Status})
		when ((Role =:= source) and (Status =:= active))
					or ((Role =:= target) and (Status =:= migrating_as_target_waiting_begin)),
				(Continuation =:= reject) or (Continuation =:= terminate),
				is_function (CompletionFun, 1) ->
	case Continuation of
		reject ->
			{reject, rejected, State};
		terminate ->
			{terminate, terminated,
					State#state{status = case Role of source -> migrating_as_source_failed; target -> migrating_as_target_failed end}}
	end;
	
begin_migration ({Role, Arguments, CompletionFun}, State = #state{status = Status})
		when (Role =:= source) or (Role =:= target), (Status =:= active) or (Status =:= migrating_as_target_waiting_begin),
				is_function (CompletionFun, 1) ->
	{reject, {invalid_arguments, Arguments}, State};
	
begin_migration ({Role, _Arguments, CompletionFun}, State = #state{status = Status})
		when (Role =:= source) or (Role =:= target), is_function (CompletionFun, 1) ->
	{reject, {invalid_status, Status}, State}.


commit_migration (State = #state{commit_migration_delegate = Delegate})
		when (Delegate =/= none) ->
	Delegate (State);
	
commit_migration (State = #state{status = migrating_as_source_waiting_commit, migration_outcome = Outcome})
		when (Outcome =:= succeed) or (Outcome =:= fail) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = migrating_as_source_succeeded}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_source_failed}}
	end;
	
commit_migration (State = #state{status = migrating_as_target_waiting_commit, migration_outcome = Outcome})
		when (Outcome =:= succeed) or (Outcome =:= fail) ->
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
		when (Outcome =:= succeed) or (Outcome =:= fail) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = active}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_source_failed}}
	end;
	
rollback_migration (State = #state{status = migrating_as_target_waiting_commit, migration_outcome = Outcome})
		when (Outcome =:= succeed) or (Outcome =:= fail) ->
	case Outcome of
		succeed ->
			{continue, State#state{status = migrating_as_target_failed}};
		fail ->
			{terminate, failed, State#state{status = migrating_as_target_failed}}
	end.
