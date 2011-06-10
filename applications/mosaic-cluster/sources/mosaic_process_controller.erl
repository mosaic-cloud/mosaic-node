
-module (mosaic_process_controller).

-behaviour (gen_server).


-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/1, start_supervised/2]).
-export ([stop/1, stop/2]).
-export ([resolve/2, fold/3, count/1]).
-export ([create/4, migrate/6, migrate/8]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start () ->
	start (defaults).

start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_process_tools:start (gen_server, mosaic_process_controller, QualifiedName, Configuration).


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_process_controller, QualifiedName, Configuration).


start_supervised (QualifiedName) ->
	start_supervised (QualifiedName, defaults).

start_supervised (QualifiedName, Configuration) ->
	mosaic_sup:start_child_process_controller (QualifiedName, Configuration).


stop (Controller) ->
	stop (Controller, normal).

stop (Controller, Signal)
		when (is_pid (Controller) orelse is_atom (Controller)) ->
	gen_server:call (Controller, {mosaic_process_controller, stop, Signal}).


resolve (Controller, Identifier)
		when (is_pid (Controller) orelse is_atom (Controller)), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	gen_server:call (Controller, {mosaic_process_controller, resolve, Identifier}).


fold (Controller, Function, InputAccumulator)
		when (is_pid (Controller) orelse is_atom (Controller)), is_function (Function, 2) ->
	gen_server:call (Controller, {mosaic_process_controller, fold, Function, InputAccumulator}).


count (Controller)
		when (is_pid (Controller) orelse is_atom (Controller)) ->
	gen_server:call (Controller, {mosaic_process_controller, count}).


create (Controller, Identifier, Module, Configuration)
		when (is_pid (Controller) orelse is_atom (Controller)),
			is_atom (Module), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	gen_server:call (Controller, {mosaic_process_controller, create, Identifier, Module, Configuration}).


migrate (SourceController, TargetController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration)
		when is_pid (SourceController), is_pid (TargetController), (SourceController =/= TargetController),
			is_binary (Identifier), (bit_size (Identifier) =:= 160), is_atom (TargetModule) ->
	gen_server:call (TargetController, {mosaic_process_controller, migrate_as_target, SourceController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration}).

migrate (SourceController, TargetController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration, Monitor, MonitorToken)
		when is_pid (SourceController), is_pid (TargetController), is_pid (Monitor),
			is_binary (Identifier), (bit_size (Identifier) =:= 160), is_atom (TargetModule),
			(SourceController =/= TargetController), (SourceController =/= Monitor), (TargetController =/= Monitor) ->
	gen_server:call (TargetController, {mosaic_process_controller, migrate_as_target, SourceController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration, Monitor, MonitorToken}).


-record (state, {qualified_name, processes, link_mapping, link_pending, migrations, migration_mapping}).
-record (process_state, {module, process}).
-record (migration_as_target_state, {migrator, migrator_token, monitor, monitor_token}).
-record (migration_as_source_state, {migrator, migrator_token}).


init ({QualifiedName, defaults}) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_process_tools:ensure_registered (QualifiedName) of
		ok ->
			State = #state{
					qualified_name = QualifiedName,
					processes = orddict:new (), link_mapping = orddict:new (), link_pending = ordsets:new (),
					migrations = orddict:new (), migration_mapping = orddict:new ()},
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State) ->
	ok.


code_change (_OldVsn, State, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_process_controller, stop, Signal}, _Sender, State) ->
	case Signal of
		normal ->
			{stop, normal, ok, State};
		_ ->
			{reply, {error, {invalid_signal, Signal}}, State}
	end;
	
handle_call (
			{mosaic_process_controller, create, Identifier, Module, Configuration}, _Sender,
			OldState = #state{processes = OldProcesses, link_mapping = OldLinkMapping})
		when is_atom (Module), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	ProcessExists = orddict:is_key (Identifier, OldProcesses),
	if
		not ProcessExists ->
			case mosaic_process:start_supervised (noname, Module, create, Identifier, Configuration) of
				{ok, Process} ->
					true = erlang:link (Process),
					ProcessState = #process_state{module = Module, process = Process},
					NewProcesses = orddict:store (Identifier, ProcessState, OldProcesses),
					NewLinkMapping = orddict:store (Process, Identifier, OldLinkMapping),
					NewState = OldState#state{processes = NewProcesses, link_mapping = NewLinkMapping},
					{reply, {ok, Process}, NewState};
				Error = {error, _Reason} ->
					{reply, Error, OldState}
			end;
		true ->
			{reply, {error, already_exists}, OldState}
	end;
	
handle_call (
			{mosaic_process_controller, migrate_as_target, SourceController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration}, Sender,
			OldState)
		when is_pid (SourceController), (SourceController =/= self ()), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_atom (TargetModule) ->
	MonitorToken = erlang:make_ref (),
	Monitor = spawn_link (
			fun () ->
				{ok, Migrator} = receive
					{mosaic_process_controller_internals, migrate, Migrator_, MonitorToken} ->
						{ok, Migrator_}
				end,
				true = erlang:link (Migrator),
				ok = receive
					{mosaic_process_migrator, migrate, MonitorToken, succeeded} ->
						_ = gen_server:reply (Sender, ok),
						ok;
					{mosaic_process_migrator, migrate, MonitorToken, failed, Reason} ->
						_ = gen_server:reply (Sender, {error, Reason}),
						ok;
					{'EXIT', Migrator, Reason} ->
						_ = gen_server:reply (Sender, {error, {unexpected_error, Reason}}),
						ok;
					Message ->
						_ = gen_server:reply (Sender, {error, {unexpected_error, {unexpected_message, Message}}}),
						ok
				end,
				true = erlang:unlink (Migrator),
				erlang:exit (normal)
			end),
	case handle_call ({mosaic_process_controller, migrate_as_target, SourceController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration, Monitor, MonitorToken}, undefined, OldState) of
		{reply, {ok, Migrator}, NewState} ->
			true = erlang:unlink (Monitor),
			Monitor ! {mosaic_process_controller_internals, migrate, Migrator, MonitorToken},
			{noreply, NewState};
		Reply = {reply, {error, _Reason}, _NewState} ->
			true = erlang:exit (Monitor, kill),
			Reply
	end;
	
handle_call (
			{mosaic_process_controller, migrate_as_target, SourceController, Identifier, SourceConfiguration, TargetModule, TargetConfiguration, Monitor, MonitorToken}, _Sender,
			OldState = #state{
					processes = OldProcesses, link_mapping = OldLinkMapping,
					migrations = OldMigrations, migration_mapping = OldMigrationMapping})
		when is_pid (SourceController), is_pid (Monitor), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_atom (TargetModule),
				(SourceController =/= self ()), (Monitor =/= self ()) ->
	ProcessExists = orddict:is_key (Identifier, OldProcesses),
	if
		not ProcessExists ->
			MigratorToken = erlang:make_ref (),
			SourceToken = erlang:make_ref (),
			TargetToken = erlang:make_ref (),
			case gen_server:call (SourceController, {mosaic_process_controller, migrate_as_source, Identifier}) of
				{ok, Source} ->
					case mosaic_process:start_supervised (noname, TargetModule, {migrate, TargetToken}, Identifier, TargetConfiguration) of
						{ok, Target} ->
							case mosaic_process_migrator:start (Source, SourceToken, Target, TargetToken, Monitor, MonitorToken) of
								{ok, Migrator} ->
									case gen_server:call (SourceController, {mosaic_process_controller, migrate_as_source, Identifier, Migrator, MigratorToken}) of
										ok ->
											case mosaic_process_migrator:migrate (Migrator, SourceConfiguration) of
												ok ->
													true = erlang:link (Migrator),
													true = erlang:link (Target),
													TargetProcessState = #process_state{
															module = TargetModule, process = Target},
													TargetMigrationState = #migration_as_target_state{
															migrator = Migrator, migrator_token = MigratorToken,
															monitor = Monitor, monitor_token = MonitorToken},
													NewProcesses = orddict:store (Identifier, TargetProcessState, OldProcesses),
													NewMigrations = orddict:store (Identifier, TargetMigrationState, OldMigrations),
													NewMigrationMapping = orddict:store (MigratorToken, Identifier, OldMigrationMapping),
													NewLinkMapping = orddict:store (Target, Identifier, orddict:store (Migrator, Identifier, OldLinkMapping)),
													NewState = OldState#state{
															processes = NewProcesses, link_mapping = NewLinkMapping,
															migrations = NewMigrations, migration_mapping = NewMigrationMapping},
													{reply, {ok, Migrator}, NewState};
												Error = {error, _Reason} ->
													true = erlang:exit (Migrator, kill),
													true = erlang:exit (Target, kill),
													{reply, Error, OldState}
											end;
										Error = {error, _Reason} ->
											true = erlang:exit (Migrator, kill),
											true = erlang:exit (Target, kill),
											{reply, Error, OldState}
									end;
								Error = {error, _Reason} ->
									Target ! {abort_migration, TargetToken},
									{reply, Error, OldState}
							end;
						Error = {error, _Reason} ->
							{reply, Error, OldState}
					end;
				Error = {error, _Reason} ->
					{reply, Error, OldState}
			end;
		true ->
			{reply, {error, already_exists}, OldState}
	end;
	
handle_call (
			{mosaic_process_controller, migrate_as_source, Identifier}, _Sender,
			State = #state{processes = Processes, migrations = Migrations})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	case orddict:find (Identifier, Processes) of
		{ok, #process_state{process = Source}} ->
			MigrationExists = orddict:is_key (Identifier, Migrations),
			if
				not MigrationExists ->
					{reply, {ok, Source}, State};
				true ->
					{reply, {error, source_already_migrating}, State}
			end;
		error ->
			{reply, {error, source_does_not_exist}, State}
	end;
	
handle_call (
			{mosaic_process_controller, migrate_as_source, Identifier, Migrator, MigratorToken}, _Sender,
			OldState = #state{
					processes = Processes, link_mapping = OldLinkMapping,
					migrations = OldMigrations, migration_mapping = OldMigrationMapping})
		when is_pid (Migrator), (Migrator =/= self ()), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	ProcessExists = orddict:is_key (Identifier, Processes),
	if
		ProcessExists ->
			MigrationExists = orddict:is_key (Identifier, OldMigrations),
			if
				not MigrationExists ->
					true = erlang:link (Migrator),
					SourceMigrationState = #migration_as_source_state{migrator = Migrator, migrator_token = MigratorToken},
					NewMigrations = orddict:store (Identifier, SourceMigrationState, OldMigrations),
					NewMigrationMapping = orddict:store (MigratorToken, Identifier, OldMigrationMapping),
					NewLinkMapping = orddict:store (Migrator, Identifier, OldLinkMapping),
					NewState = OldState#state{
							link_mapping = NewLinkMapping,
							migrations = NewMigrations, migration_mapping = NewMigrationMapping},
					{reply, ok, NewState};
				true ->
					{reply, {error, source_already_migrating}, OldState}
			end;
		true ->
			{reply, {error, source_does_not_exist}, OldState}
	end;
	
handle_call (
			{mosaic_process_controller, resolve, Identifier}, _Sender,
			State = #state{processes = Processes})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	case orddict:find (Identifier, Processes) of
		{ok, #process_state{process = Process}} ->
			{reply, {ok, Process}, State};
		error ->
			{reply, {error, process_does_not_exist}, State}
	end;
	
handle_call (
			{mosaic_process_controller, fold, Function, InputAccumulator}, _Sender,
			State = #state{processes = Processes})
		when is_function (Function, 2) ->
	OutputAccumulator = orddict:fold (
			fun (Identifier, #process_state{process = Process}, CurrentAccumulator) ->
				Function ({Identifier, Process}, CurrentAccumulator)
			end, InputAccumulator,
			Processes),
	{reply, {ok, OutputAccumulator}, State};
	
handle_call (
			{mosaic_process_controller, count}, _Sender,
			State = #state{processes = Processes}) ->
	Count = orddict:size (Processes),
	{reply, {ok, Count}, State};
	
handle_call (Request, Sender, State) ->
	ok = mosaic_transcript:trace_error ("received invalid call request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast (Request, State) ->
	ok = mosaic_transcript:trace_error ("received invalid cast request; ignoring!", [{request, Request}]),
	{noreply, State}.


handle_info (
			{'EXIT', Link, Reason},
			OldState = #state{
					processes = OldProcesses, link_mapping = OldLinkMapping, link_pending = OldLinkPending,
					migrations = OldMigrations, migration_mapping = OldMigrationMapping})
		when is_pid (Link) ->
	case orddict:find (Link, OldLinkMapping) of
		{ok, Identifier} ->
			{ok, #process_state{process = Process}} = orddict:find (Identifier, OldProcesses),
			{ok, NewProcesses1, NewLinkMapping1, NewLinkPending1} = if
				Link =:= Process ->
					NewProcesses1_ = orddict:erase (Identifier, OldProcesses),
					NewLinkMapping1_ = orddict:erase (Process, OldLinkMapping),
					{ok, NewProcesses1_, NewLinkMapping1_, OldLinkPending};
				Link =/= Process, Reason =/= normal ->
					NewProcesses1_ = orddict:erase (Identifier, OldProcesses),
					NewLinkMapping1_ = orddict:erase (Process, OldLinkMapping),
					NewLinkPending1_ = ordsets:add_element (Process, OldLinkPending),
					{ok, NewProcesses1_, NewLinkMapping1_, NewLinkPending1_};
				Link =/= Process, Reason =:= normal ->
					{ok, OldProcesses, OldLinkMapping, OldLinkPending}
			end,
			{ok, NewLinkMapping2, NewLinkPending2, NewMigrations, NewMigrationMapping} = case orddict:find (Identifier, OldMigrations) of
				{ok, OldMigrationState} ->
					{ok, Migrator, MigratorToken} = case OldMigrationState of
						#migration_as_target_state{migrator = Migrator_, migrator_token = MigratorToken_} ->
							{ok, Migrator_, MigratorToken_};
						#migration_as_source_state{migrator = Migrator_, migrator_token = MigratorToken_} ->
							{ok, Migrator_, MigratorToken_}
					end,
					NewLinkMapping2_ = orddict:erase (Migrator, NewLinkMapping1),
					NewLinkPending2_ = if Link =/= Migrator -> ordsets:add_element (Migrator, NewLinkPending1); true -> NewLinkPending1 end,
					NewMigrations2_ = orddict:erase (Identifier, OldMigrations),
					NewMigrationMapping2_ = orddict:erase (MigratorToken, OldMigrationMapping),
					{ok, NewLinkMapping2_, NewLinkPending2_, NewMigrations2_, NewMigrationMapping2_};
				error ->
					{ok, NewLinkMapping1, NewLinkPending1, OldMigrations, OldMigrationMapping}
			end,
			NewState = OldState#state{
					processes = NewProcesses1, link_mapping = NewLinkMapping2, link_pending = NewLinkPending2,
					migrations = NewMigrations, migration_mapping = NewMigrationMapping},
			{noreply, NewState};
		error ->
			LinkPending = ordsets:is_element (Link, OldLinkPending),
			if
				LinkPending =:= true ->
					NewLinkPending = ordsets:del_element (Link, OldLinkPending),
					NewState = OldState#state{link_pending = NewLinkPending},
					{noreply, NewState};
				true ->
					ok = mosaic_transcript:trace_error ("received unexpected link exit; ignoring!", [{link, Link}, {reason, Reason}]),
					{noreply, OldState}
			end
	end;
	
handle_info (Message, State) ->
	ok = mosaic_transcript:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{noreply, State}.
