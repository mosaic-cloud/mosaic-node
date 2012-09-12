
-module (mosaic_process_migrator).

-behaviour (gen_fsm).


-export ([start/6, start/7, start_link/6, start_link/7]).
-export ([migrate/2]).
-export ([init/1, terminate/3, code_change/4, handle_sync_event/4, handle_event/3, handle_info/3]).
-export ([
		source_begin_waiting/2, source_begin_succeeded_waiting/2,
		source_prepared_waiting/2,
		target_begin_waiting/2, target_begin_succeeded_waiting/2,
		peers_completed_waiting/2,
		source_commit_waiting/2, source_commit_succeeded_waiting/2,
		target_commit_waiting/2, target_commit_succeeded_waiting/2]).


start (Source, SourceToken, Target, TargetToken, Monitor, MonitorToken) ->
	start (noname, Source, SourceToken, Target, TargetToken, Monitor, MonitorToken).

start (QualifiedName, Source, SourceToken, Target, TargetToken, Monitor, MonitorToken)
		when is_pid (Source), is_pid (Target), is_pid (Monitor), (Source =/= Target), (Source =/= Monitor), (Target =/= Monitor),
				(SourceToken =/= TargetToken), (SourceToken =/= MonitorToken), (TargetToken =/= MonitorToken) ->
	mosaic_process_tools:start (gen_fsm, mosaic_process_migrator, QualifiedName, {Source, SourceToken, Target, TargetToken, Monitor, MonitorToken}).


start_link (Source, SourceToken, Target, TargetToken, Monitor, MonitorToken) ->
	start_link (noname, Source, SourceToken, Target, TargetToken, Monitor, MonitorToken).

start_link (QualifiedName, Source, SourceToken, Target, TargetToken, Monitor, MonitorToken)
		when is_pid (Source), is_pid (Target), is_pid (Monitor), (Source =/= Target), (Source =/= Monitor), (Target =/= Monitor),
				(SourceToken =/= TargetToken), (SourceToken =/= MonitorToken), (TargetToken =/= MonitorToken) ->
	mosaic_process_tools:start_link (gen_fsm, mosaic_process_migrator, QualifiedName, {Source, SourceToken, Target, TargetToken, Monitor, MonitorToken}).


migrate (Migrator, Configuration)
		when is_pid (Migrator) ->
	gen_fsm:sync_send_all_state_event (Migrator, {mosaic_process_migrator, migrate, Configuration}).


-record (state, {source, source_token, target, target_token, monitor, monitor_token, source_completed, target_completed}).


init ({QualifiedName, {Source, SourceToken, Target, TargetToken, Monitor, MonitorToken}})
		when is_pid (Source), is_pid (Target), is_pid (Monitor), (Source =/= Target), (Source =/= Monitor), (Target =/= Monitor),
				(SourceToken =/= TargetToken), (SourceToken =/= MonitorToken), (TargetToken =/= MonitorToken) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_process_tools:ensure_registered (QualifiedName) of
		ok ->
			StateData = #state{
					source = Source, source_token = SourceToken,
					target = Target, target_token = TargetToken,
					monitor = Monitor, monitor_token = MonitorToken,
					source_completed = false, target_completed = false},
			{ok, migrate_waiting, StateData};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (Reason, _StateName, _StateData = #state{monitor = Monitor, monitor_token = MonitorToken}) ->
	case Reason of
		normal ->
			Monitor ! {mosaic_process_migrator, migrate, MonitorToken, succeeded},
			ok;
		{error, ErrorReason} ->
			Monitor ! {mosaic_process_migrator, migrate, MonitorToken, failed, ErrorReason},
			ok;
		_ ->
			Monitor ! {mosaic_process_migrator, migrate, MonitorToken, failed, {unexpected_reason, Reason}},
			ok
	end.


code_change (_OldVsn, StateName, StateData, _Arguments) ->
	{ok, StateName, StateData}.


handle_sync_event ({mosaic_process_migrator, migrate, Configuration}, _Sender, migrate_waiting, StateData) ->
	ok = send_event ({mosaic_process_migrator_internals, source_begin, Configuration}),
	{reply, ok, source_begin_waiting, StateData};
	
handle_sync_event (Request, _Sender, _StateName, StateData) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, StateData}.


handle_event (Request, _StateName, StateData) ->
	{stop, {error, {invalid_request, Request}}, StateData}.


source_begin_waiting ({mosaic_process_migrator_internals, source_begin, Configuration}, StateData = #state{source = Source, source_token = SourceToken}) ->
	case mosaic_process:begin_migration (Source, SourceToken, Configuration, erlang:self ()) of
		ok ->
			{next_state, source_begin_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {error, {migration_failed, source, 'begin', Reason}}, StateData}
	end.


source_begin_succeeded_waiting ({mosaic_process_migrator, 'begin', SourceToken, succeeded}, StateData = #state{source_token = SourceToken}) ->
	{next_state, source_prepared_waiting, StateData}.


source_prepared_waiting ({mosaic_process_migrator, continue, SourceToken, {prepared, Configuration}}, StateData = #state{source_token = SourceToken}) ->
	ok = send_event ({mosaic_process_migrator_internals, target_begin, Configuration}),
	{next_state, target_begin_waiting, StateData}.


target_begin_waiting ({mosaic_process_migrator_internals, target_begin, Configuration}, StateData = #state{target = Target, target_token = TargetToken}) ->
	case mosaic_process:begin_migration (Target, TargetToken, Configuration, erlang:self ()) of
		ok ->
			{next_state, target_begin_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {error, {migration_failed, target, 'begin', Reason}}, StateData}
	end;
	
target_begin_waiting ({mosaic_process_migrator, continue, SourceToken, completed}, StateData = #state{source_token = SourceToken}) ->
	continue_migration (target_begin_waiting, source, StateData).


target_begin_succeeded_waiting ({mosaic_process_migrator, 'begin', TargetToken, succeeded}, StateData = #state{target_token = TargetToken}) ->
	{next_state, peers_completed_waiting, StateData};
	
target_begin_succeeded_waiting ({mosaic_process_migrator, continue, SourceToken, completed}, StateData = #state{source_token = SourceToken}) ->
	continue_migration (target_begin_succeeded_waiting, source, StateData).


peers_completed_waiting ({mosaic_process_migrator, continue, SourceToken, completed}, StateData = #state{source_token = SourceToken}) ->
	continue_migration (peers_completed_waiting, source, StateData);
	
peers_completed_waiting ({mosaic_process_migrator, continue, TargetToken, completed}, StateData = #state{target_token = TargetToken}) ->
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
			ok = send_event ({mosaic_process_migrator_internals, target_commit}),
			{next_state, target_commit_waiting, NewStateData};
		true ->
			{next_state, peers_completed_waiting, NewStateData}
	end;
	
continue_migration (peers_completed_waiting, target, OldStateData = #state{source_completed = SourceCompleted, target_completed = false}) ->
	NewStateData = OldStateData#state{target_completed = true},
	if
		SourceCompleted ->
			ok = send_event ({mosaic_process_migrator_internals, target_commit}),
			{next_state, target_commit_waiting, NewStateData};
		true ->
			{next_state, peers_completed_waiting, NewStateData}
	end.


target_commit_waiting ({mosaic_process_migrator_internals, target_commit}, StateData = #state{target = Target, target_token = TargetToken}) ->
	case mosaic_process:commit_migration (Target, TargetToken) of
		ok ->
			{next_state, target_commit_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {error, {migration_failed, target, commit, Reason}}, StateData}
	end.


target_commit_succeeded_waiting ({mosaic_process_migrator, commit, TargetToken, succeeded}, StateData = #state{target_token = TargetToken}) ->
	ok = send_event ({mosaic_process_migrator_internals, source_commit}),
	{next_state, source_commit_waiting, StateData}.


source_commit_waiting ({mosaic_process_migrator_internals, source_commit}, StateData = #state{source = Source, source_token = SourceToken}) ->
	case mosaic_process:commit_migration (Source, SourceToken) of
		ok ->
			{next_state, source_commit_succeeded_waiting, StateData};
		{error, Reason} ->
			{stop, {error, {migration_failed, source, commit, Reason}}, StateData}
	end.


source_commit_succeeded_waiting ({mosaic_process_migrator, commit, SourceToken, succeeded}, StateData = #state{source_token = SourceToken}) ->
	{stop, normal, StateData}.


handle_info (Message, StateName, StateData = #state{source_token = SourceToken, target_token = TargetToken}) ->
	Action = case Message of
		{mosaic_process_migrator, 'begin', SourceToken, succeeded} ->
			send_event;
		{mosaic_process_migrator, 'begin', SourceToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, 'begin', TargetToken, succeeded} ->
			send_event;
		{mosaic_process_migrator, 'begin', TargetToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, continue, SourceToken, {prepared, _Configuration}} ->
			send_event;
		{mosaic_process_migrator, continue, SourceToken, completed} ->
			send_event;
		{mosaic_process_migrator, continue, SourceToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, continue, TargetToken, completed} ->
			send_event;
		{mosaic_process_migrator, continue, TargetToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, commit, SourceToken, succeeded} ->
			send_event;
		{mosaic_process_migrator, commit, SourceToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, commit, TargetToken, succeeded} ->
			send_event;
		{mosaic_process_migrator, commit, TargetToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, rollback, SourceToken, succeeded} ->
			send_event;
		{mosaic_process_migrator, rollback, SourceToken, failed, _Reason} ->
			send_event;
		{mosaic_process_migrator, rollback, TargetToken, succeeded} ->
			send_event;
		{mosaic_process_migrator, rollback, TargetToken, failed, _Reason} ->
			send_event;
		_ ->
			stop
	end,
	case Action of
		send_event ->
			ok = send_event (Message),
			{next_state, StateName, StateData};
		stop ->
			{stop, {error, {invalid_message, Message}}, StateData}
	end.


send_event (Event) ->
	gen_fsm:send_event (erlang:self (), Event).
