
-module (mosaic_process_migrator).

-behaviour (gen_fsm).

-export ([start/6, start/7, start_link/6, start_link/7]).
-export ([migrate/2]).
-export ([init/1, terminate/3, code_change/4, handle_sync_event/4, handle_event/3, handle_info/3]).
-export ([
		source_begin_waiting/3]).
-export ([
		source_begin_waiting/2, source_begin_succeeded_waiting/2,
		source_prepared_waiting/2,
		target_begin_waiting/2, target_begin_succeeded_waiting/2,
		peers_completed_waiting/2,
		source_commit_waiting/2, source_commit_succeeded_waiting/2,
		target_commit_waiting/2, target_commit_succeeded_waiting/2]).


start (Source, SourceToken, Target, TargetToken, Observer, ObserverToken) ->
	start (noname, Source, SourceToken, Target, TargetToken, Observer, ObserverToken).

start (QualifiedName, Source, SourceToken, Target, TargetToken, Observer, ObserverToken)
		when is_pid (Source), is_pid (Target), is_pid (Observer), (Source =/= Target), (Source =/= Observer), (Target =/= Observer),
				(SourceToken =/= TargetToken), (SourceToken =/= ObserverToken), (TargetToken =/= ObserverToken) ->
	mosaic_tools:start (gen_fsm, mosaic_process_migrator, QualifiedName, {Source, SourceToken, Target, TargetToken, Observer, ObserverToken}).


start_link (Source, SourceToken, Target, TargetToken, Observer, ObserverToken) ->
	start_link (noname, Source, SourceToken, Target, TargetToken, Observer, ObserverToken).

start_link (QualifiedName, Source, SourceToken, Target, TargetToken, Observer, ObserverToken)
		when is_pid (Source), is_pid (Target), is_pid (Observer), (Source =/= Target), (Source =/= Observer), (Target =/= Observer),
				(SourceToken =/= TargetToken), (SourceToken =/= ObserverToken), (TargetToken =/= ObserverToken) ->
	mosaic_tools:start (gen_fsm, mosaic_process_migrator, QualifiedName, {Source, SourceToken, Target, TargetToken, Observer, ObserverToken}).


migrate (Migrator, Arguments)
		when is_pid (Migrator) ->
	gen_fsm:sync_send_event (Migrator, {migrate, Arguments}).


-record (state, {source, source_token, target, target_token, observer, observer_token, source_completed, target_completed}).


init ({QualifiedName, {Source, SourceToken, Target, TargetToken, Observer, ObserverToken}})
		when is_pid (Source), is_pid (Target), is_pid (Observer), (Source =/= Target), (Source =/= Observer), (Target =/= Observer),
				(SourceToken =/= TargetToken), (SourceToken =/= ObserverToken), (TargetToken =/= ObserverToken) ->
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			false = erlang:process_flag (trap_exit, true),
			StateData = #state{
					source = Source, source_token = SourceToken,
					target = Target, target_token = TargetToken,
					observer = Observer, observer_token = ObserverToken,
					source_completed = false, target_completed = false},
			{ok, source_begin_waiting, StateData};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (Reason, _StateName, _StateData = #state{observer = Observer, observer_token = ObserverToken}) ->
	Observer ! {terminate, ObserverToken, Reason},
	ok.


code_change (_OldVsn, StateName, StateData, _Arguments) ->
	{ok, StateName, StateData}.


handle_sync_event (Event, _Sender, StateName, StateData) ->
	ok = mosaic_tools:report_error (mosaic_process_migrator, handle_sync_event, invalid_event, {Event}),
	{reply, {error, invalid_event}, StateName, StateData}.


handle_event (Event, StateName, StateData) ->
	ok = mosaic_tools:report_error (mosaic_process_migrator, handle_event, invalid_event, {Event}),
	{next_state, StateName, StateData}.


source_begin_waiting ({migrate, Arguments}, _Sender, StateData) ->
	ok = trigger_event ({source_begin, Arguments}),
	{reply, ok, source_begin_waiting, StateData}.

source_begin_waiting ({source_begin, Arguments}, StateData = #state{source = Source, source_token = SourceToken}) ->
	case mosaic_process:begin_migration (Source, SourceToken, Arguments, erlang:self ()) of
		ok ->
			{next_state, source_begin_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {migration_failed, source, 'begin', Reason}, StateData}
	end.


source_begin_succeeded_waiting ({begin_migration, SourceToken, succeeded}, StateData = #state{source_token = SourceToken}) ->
	{next_state, source_prepared_waiting, StateData}.


source_prepared_waiting ({continue_migration, SourceToken, prepared, Arguments}, StateData = #state{source_token = SourceToken}) ->
	ok = trigger_event ({target_begin, Arguments}),
	{next_state, target_begin_waiting, StateData}.


target_begin_waiting ({target_begin, Arguments}, StateData = #state{target = Target, target_token = TargetToken}) ->
	case mosaic_process:begin_migration (Target, TargetToken, Arguments, erlang:self ()) of
		ok ->
			{next_state, target_begin_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {migration_failed, target, 'begin', Reason}, StateData}
	end;
	
target_begin_waiting ({continue_migration, SourceToken, completed}, StateData = #state{source_token = SourceToken}) ->
	continue_migration (target_begin_waiting, source, StateData).


target_begin_succeeded_waiting ({begin_migration, TargetToken, succeeded}, StateData = #state{target_token = TargetToken}) ->
	{next_state, peers_completed_waiting, StateData};
	
target_begin_succeeded_waiting ({continue_migration, SourceToken, completed}, StateData = #state{source_token = SourceToken}) ->
	continue_migration (target_begin_succeeded_waiting, source, StateData).


peers_completed_waiting ({continue_migration, SourceToken, completed}, StateData = #state{source_token = SourceToken}) ->
	continue_migration (peers_completed_waiting, source, StateData);
	
peers_completed_waiting ({continue_migration, TargetToken, completed}, StateData = #state{target_token = TargetToken}) ->
	continue_migration (peers_completed_waiting, target, StateData).


continue_migration (target_begin_waiting, source, OldStateData = #state{source_completed = false, target_completed = false}) ->
	{next_state, target_begin_waiting, OldStateData#state{source_completed = true}};
	
continue_migration (target_begin_succeeded_waiting, source, OldStateData = #state{source_completed = false, target_completed = false}) ->
	NewStateData = OldStateData#state{source_completed = true},
	{next_state, target_begin_succeeded_waiting, NewStateData};
	
continue_migration (peers_completed_waiting, source, OldStateData = #state{source_completed = false, target_completed = TargetCompleted}) ->
	NewStateData = OldStateData#state{source_completed = true},
	if
		TargetCompleted ->
			ok = trigger_event (target_commit),
			{next_state, target_commit_waiting, NewStateData};
		true ->
			{next_state, peers_completed_waiting, NewStateData}
	end;
	
continue_migration (peers_completed_waiting, target, OldStateData = #state{source_completed = SourceCompleted, target_completed = false}) ->
	NewStateData = OldStateData#state{target_completed = true},
	if
		SourceCompleted ->
			ok = trigger_event (target_commit),
			{next_state, target_commit_waiting, NewStateData};
		true ->
			{next_state, peers_completed_waiting, NewStateData}
	end.


target_commit_waiting (target_commit, StateData = #state{target = Target, target_token = TargetToken}) ->
	case mosaic_process:commit_migration (Target, TargetToken) of
		ok ->
			{next_state, target_commit_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {migration_failed, target, commit, Reason}, StateData}
	end.


target_commit_succeeded_waiting ({commit_migration, TargetToken, succeeded}, StateData = #state{target_token = TargetToken}) ->
	ok = trigger_event (source_commit),
	{next_state, source_commit_waiting, StateData}.


source_commit_waiting (source_commit, StateData = #state{source = Source, source_token = SourceToken}) ->
	case mosaic_process:commit_migration (Source, SourceToken) of
		ok ->
			{next_state, source_commit_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {migration_failed, source, commit, Reason}, StateData}
	end.


source_commit_succeeded_waiting ({commit_migration, SourceToken, succeeded}, StateData = #state{source_token = SourceToken}) ->
	{stop, normal, StateData}.


handle_info (Event, StateName, StateData = #state{source_token = SourceToken, target_token = TargetToken}) ->
	Action = case Event of
		{begin_migration, SourceToken, succeeded} ->
			send_event;
		{begin_migration, SourceToken, failed, _Reason} ->
			send_event;
		{begin_migration, TargetToken, succeeded} ->
			send_event;
		{begin_migration, TargetToken, failed, _Reason} ->
			send_event;
		{continue_migration, SourceToken, prepared, _Arguments} ->
			send_event;
		{continue_migration, SourceToken, completed} ->
			send_event;
		{continue_migration, SourceToken, failed, _Reason} ->
			send_event;
		{continue_migration, TargetToken, completed} ->
			send_event;
		{continue_migration, TargetToken, failed, _Reason} ->
			send_event;
		{commit_migration, SourceToken, succeeded} ->
			send_event;
		{commit_migration, SourceToken, failed, _Reason} ->
			send_event;
		{commit_migration, TargetToken, succeeded} ->
			send_event;
		{commit_migration, TargetToken, failed, _Reason} ->
			send_event;
		{rollback_migration, SourceToken, succeeded} ->
			send_event;
		{rollback_migration, SourceToken, failed, _Reason} ->
			send_event;
		{rollback_migration, TargetToken, succeeded} ->
			send_event;
		{rollback_migration, TargetToken, failed, _Reason} ->
			send_event
	end,
	case Action of
		send_event ->
			ok = trigger_event (Event),
			{next_state, StateName, StateData}
	end.


trigger_event (Event) ->
	gen_fsm:send_event (erlang:self (), Event).
