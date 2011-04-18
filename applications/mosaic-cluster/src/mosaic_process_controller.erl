
-module (mosaic_process_controller).

-behaviour (gen_server).

-export ([start/2, start_link/2, start_supervised/2]).
-export ([stop/1, stop/2, stop/3]).
-export ([create/4, migrate/3, migrate/4, migrate/5, migrate/6, fold/3, count/1]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start (Name, Configuration)
		when is_atom (Name) ->
	gen_server:start ({local, Name}, mosaic_process_controller, {Name, Configuration}, []).

start_link (Name, Configuration)
		when is_atom (Name) ->
	gen_server:start_link ({local, Name}, mosaic_process_controller, {Name, Configuration}, []).

start_supervised (Name, Configuration)
		when is_atom (Name) ->
	mosaic_cluster_sup:start_child_process_controller (Name, Configuration).

stop (Controller) ->
	stop (Controller, normal).

stop (Controller, Reason)
		when (is_atom (Controller) or is_pid (Controller)) ->
	gen_server:call (Controller, {stop, Reason}).

stop (Controller, Key, Signal)
		when (is_atom (Controller) or is_pid (Controller)) ->
	gen_server:call (Controller, {stop, Key, Signal}).

create (Controller, Key, Module, Arguments)
		when (is_pid (Controller) or is_atom (Controller)), is_atom (Module) ->
	gen_server:call (Controller, {create, Key, Module, Arguments}).

migrate (SourceController, TargetController, Key) ->
	migrate (SourceController, TargetController, Key, defaults).

migrate (SourceController, TargetController, Key, Arguments)
		when is_pid (SourceController), is_pid (TargetController), (SourceController =/= TargetController) ->
	gen_server:call (TargetController, {migrate_as_target, SourceController, Key, Arguments}).

migrate (SourceController, TargetController, Key, Observer, ObserverToken) ->
	migrate (SourceController, TargetController, Key, defaults, Observer, ObserverToken).

migrate (SourceController, TargetController, Key, Arguments, Observer, ObserverToken)
		when is_pid (SourceController), is_pid (TargetController), is_pid (Observer),
				(SourceController =/= TargetController), (SourceController =/= Observer), (TargetController =/= Observer) ->
	gen_server:call (TargetController, {migrate_as_target, SourceController, Key, Arguments, Observer, ObserverToken}).

fold (Controller, Function, InputAccumulator)
		when is_pid (Controller), is_function (Function, 2) ->
	gen_server:call (Controller, {fold, Function, InputAccumulator}).

count (Controller)
		when is_pid (Controller) ->
	gen_server:call (Controller, {count}).


-record (state, {name, processes, link_mapping, link_pending, migrations, migration_mapping, process_supervisor}).
-record (process_state, {name, module, process}).
-record (migration_as_target_state, {migrator, migrator_token, observer, observer_token}).
-record (migration_as_source_state, {migrator, migrator_token}).


init ({Name, defaults})
		when is_atom (Name) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_tools:ensure_registered (Name, erlang:self ()) of
		ok ->
			case erlang:whereis (mosaic_process_sup) of
				ProcessSupervisor when is_pid (ProcessSupervisor) ->
					State = #state{
							name = Name,
							processes = orddict:new (), link_mapping = orddict:new (), link_pending = ordsets:new (),
							migrations = orddict:new (), migration_mapping = orddict:new (),
							process_supervisor = ProcessSupervisor},
					{ok, State};
				Port when is_port (Port) ->
					{stop, process_supervisor_is_invalid};
				undefined ->
					{stop, process_supervisor_does_not_exist}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State) ->
	ok.


code_change (_OldVsn, State, _Data) ->
	{ok, State}.


handle_call ({stop, Signal}, _Sender, State) ->
	case Signal of
		normal ->
			{stop, normal, ok, State};
		_ ->
			{reply, {error, invalid_signal}, State}
	end;
	
handle_call (
			{create, Key, Module, Arguments}, _Sender,
			OldState = #state{processes = OldProcesses, link_mapping = OldLinkMapping,
			process_supervisor = ProcessSupervisor})
		when is_atom (Module) ->
	ProcessExists = orddict:is_key (Key, OldProcesses),
	if
		not ProcessExists ->
			{ok, Name} = generate_process_name (Key, Module),
			case mosaic_process:start_supervised (ProcessSupervisor, Name, Module, {create, Arguments}) of
				{ok, Process} ->
					true = erlang:link (Process),
					ProcessState = #process_state{name = Name, module = Module, process = Process},
					NewProcesses = orddict:store (Key, ProcessState, OldProcesses),
					NewLinkMapping = orddict:store (Process, Key, OldLinkMapping),
					NewState = OldState#state{processes = NewProcesses, link_mapping = NewLinkMapping},
					{reply, {ok, Process}, NewState};
				Error = {error, _Reason} ->
					{reply, Error, OldState}
			end;
		true ->
			{reply, {error, already_exists}, OldState}
	end;
	
handle_call (
			{stop, Key, Signal}, _Sender,
			State = #state{processes = Processes}) ->
	case orddict:find (Key, Processes) of
		{ok, #process_state{process = Process}} ->
			Outcome = mosaic_process:stop (Process, Signal),
			{reply, Outcome, State};
		error ->
			{reply, {error, process_does_not_exist}, State}
	end;
	
handle_call (
			{migrate_as_target, SourceController, Key, Arguments}, Sender,
			OldState)
		when is_pid (SourceController) ->
	WaiterToken = erlang:make_ref (),
	Waiter = spawn_link (
			fun () ->
				{ok, Migrator, Target} = receive
					{'begin', Migrator_, Target_, WaiterToken} ->
						{ok, Migrator_, Target_}
				end,
				true = erlang:link (Migrator),
				ok = receive
					{terminate, WaiterToken, normal} ->
						_ = gen_server:reply (Sender, {ok, Target}),
						ok;
					{terminate, WaiterToken, Reason} ->
						_ = gen_server:reply (Sender, {error, Reason}),
						ok;
					{'EXIT', Migrator, Reason} ->
						_ = gen_server:reply (Sender, {error, Reason}),
						ok;
					Message ->
						_ = gen_server:reply (Sender, {error, {unexpected_message, Message}}),
						ok
				end,
				true = erlang:unlink (Migrator),
				erlang:exit (normal)
			end),
	case handle_call ({migrate_as_target, SourceController, Key, Arguments, Waiter, WaiterToken}, undefined, OldState) of
		{reply, {ok, Migrator, Target}, NewState} ->
			true = erlang:unlink (Waiter),
			Waiter ! {'begin', Migrator, Target, WaiterToken},
			{noreply, NewState};
		Reply = {reply, {error, _Reason}, _NewState} ->
			true = erlang:exit (Waiter, kill),
			Reply
	end;
	
handle_call (
			{migrate_as_target, SourceController, Key, Arguments, Observer, ObserverToken}, _Sender,
			OldState = #state{
					processes = OldProcesses, link_mapping = OldLinkMapping,
					migrations = OldMigrations, migration_mapping = OldMigrationMapping,
					process_supervisor = ProcessSupervisor})
		when is_pid (SourceController), is_pid (Observer), (SourceController =/= self ()), (Observer =/= self ()) ->
	ProcessExists = orddict:is_key (Key, OldProcesses),
	if
		not ProcessExists ->
			MigratorToken = erlang:make_ref (),
			SourceToken = erlang:make_ref (),
			TargetToken = erlang:make_ref (),
			case gen_server:call (SourceController, {migrate_as_source, Key}) of
				{ok, Module, Source} ->
					{ok, TargetName} = generate_process_name (Key, Module),
					case mosaic_process:start_supervised (ProcessSupervisor, TargetName, Module, {migrate, TargetToken}) of
						{ok, Target} ->
							case mosaic_process_migrator:start (Source, SourceToken, Target, TargetToken, Observer, ObserverToken) of
								{ok, Migrator} ->
									case gen_server:call (SourceController, {migrate_as_source, Key, Migrator, MigratorToken}) of
										ok ->
											case mosaic_process_migrator:migrate (Migrator, Arguments) of
												ok ->
													%true = erlang:link (Migrator),
													true = erlang:link (Target),
													TargetProcessState = #process_state{
															name = TargetName, module = Module, process = Target},
													TargetMigrationState = #migration_as_target_state{
															migrator = Migrator, migrator_token = MigratorToken,
															observer = Observer, observer_token = ObserverToken},
													NewProcesses = orddict:store (Key, TargetProcessState, OldProcesses),
													NewMigrations = orddict:store (Key, TargetMigrationState, OldMigrations),
													NewMigrationMapping = orddict:store (MigratorToken, Key, OldMigrationMapping),
													NewLinkMapping = orddict:store (Target, Key, orddict:store (Migrator, Key, OldLinkMapping)),
													NewState = OldState#state{
															processes = NewProcesses, link_mapping = NewLinkMapping,
															migrations = NewMigrations, migration_mapping = NewMigrationMapping},
													{reply, {ok, Migrator, Target}, NewState};
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
			{migrate_as_source, Key}, _Sender,
			State = #state{processes = Processes, migrations = Migrations}) ->
	case orddict:find (Key, Processes) of
		{ok, #process_state{module = Module, process = Source}} ->
			MigrationExists = orddict:is_key (Key, Migrations),
			if
				not MigrationExists ->
					{reply, {ok, Module, Source}, State};
				true ->
					{reply, {error, source_already_migrating}, State}
			end;
		error ->
			{reply, {error, source_does_not_exist}, State}
	end;
	
handle_call (
			{migrate_as_source, Key, Migrator, MigratorToken}, _Sender,
			OldState = #state{
					processes = Processes, link_mapping = OldLinkMapping,
					migrations = OldMigrations, migration_mapping = OldMigrationMapping})
		when is_pid (Migrator), (Migrator =/= self ()) ->
	ProcessExists = orddict:is_key (Key, Processes),
	if
		ProcessExists ->
			MigrationExists = orddict:is_key (Key, OldMigrations),
			if
				not MigrationExists ->
					%true = erlang:link (Migrator),
					SourceMigrationState = #migration_as_source_state{migrator = Migrator, migrator_token = MigratorToken},
					NewMigrations = orddict:store (Key, SourceMigrationState, OldMigrations),
					NewMigrationMapping = orddict:store (MigratorToken, Key, OldMigrationMapping),
					NewLinkMapping = orddict:store (Migrator, Key, OldLinkMapping),
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
			{fold, Function, InputAccumulator}, _Sender,
			State = #state{processes = Processes})
		when is_function (Function, 2) ->
	OutputAccumulator = orddict:fold (
			fun (Key, #process_state{process = Process}, CurrentAccumulator) ->
				Function ({Key, Process}, CurrentAccumulator)
			end, InputAccumulator,
			Processes),
	{reply, {ok, OutputAccumulator}, State};
	
handle_call (
			{count}, _Sender,
			State = #state{processes = Processes}) ->
	Count = orddict:size (Processes),
	{reply, {ok, Count}, State};
	
handle_call (Request, _Sender, State) ->
	ok = mosaic_tools:report_error (mosaic_process_controller, handle_call, invalid_request, {Request}),
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast (Request, State) ->
	ok = mosaic_tools:report_error (mosaic_process_controller, handle_cast, invalid_request, {Request}),
	{noreply, State}.


handle_info ({'EXIT', ProcessSupervisor, _Reason}, State = #state{process_supervisor = ProcessSupervisor}) ->
	{stop, process_supervisor_exited, State};
	
handle_info (
			{'EXIT', Link, _Reason},
			OldState = #state{
					processes = OldProcesses, link_mapping = OldLinkMapping, link_pending = OldLinkPending,
					migrations = OldMigrations, migration_mapping = OldMigrationMapping})
		when is_pid (Link) ->
	case orddict:find (Link, OldLinkMapping) of
		{ok, Key} ->
			{ok, #process_state{process = Process}} = orddict:find (Key, OldProcesses),
			% ok = error_logger:info_report ([{process_exited, Key, Process, Reason}]),
			NewProcesses = orddict:erase (Key, OldProcesses),
			NewLinkMapping1 = orddict:erase (Process, OldLinkMapping),
			NewLinkPending1 = if Link =/= Process -> ordsets:add_element (Process, OldLinkPending); true -> OldLinkPending end,
			{NewLinkMapping2, NewLinkPending2, NewMigrations, NewMigrationMapping} = case orddict:find (Key, OldMigrations) of
				{ok, OldMigrationState} ->
					{Migrator, MigratorToken} = case OldMigrationState of
						#migration_as_target_state{migrator = Migrator_, migrator_token = MigratorToken_} ->
							{Migrator_, MigratorToken_};
						#migration_as_source_state{migrator = Migrator_, migrator_token = MigratorToken_} ->
							{Migrator_, MigratorToken_}
					end,
					NewMigrations_ = orddict:erase (Key, OldMigrations),
					NewMigrationMapping_ = orddict:erase (MigratorToken, OldMigrationMapping),
					NewLinkMapping2_ = if Link =/= Migrator -> orddict:erase (Migrator, NewLinkMapping1); true -> NewLinkMapping1 end,
					NewLinkPending2_ = if Link =/= Migrator -> ordsets:add_element (Migrator, NewLinkPending1); true -> NewLinkPending1 end,
					{NewLinkMapping2_, NewLinkPending2_, NewMigrations_, NewMigrationMapping_};
				error ->
					{NewLinkMapping1, NewLinkPending1, OldMigrations, OldMigrationMapping}
			end,
			NewState = OldState#state{
					processes = NewProcesses, link_mapping = NewLinkMapping2, link_pending = NewLinkPending2,
					migrations = NewMigrations, migration_mapping = NewMigrationMapping},
			{noreply, NewState};
		error ->
			LinkPending = ordsets:is_element (Link, OldLinkPending),
			if
				LinkPending ->
					NewLinkPending = ordsets:del_element (Link, OldLinkPending),
					NewState = OldState#state{link_pending = NewLinkPending},
					{noreply, NewState};
				true ->
					ok = mosaic_tools:report_error (mosaic_process_controller, handle_info, invalid_link, {Link}),
					{noreply, OldState}
			end
	end;
	
handle_info (Message, State) ->
	ok = mosaic_tools:report_error (mosaic_process_controller, handle_info, invalid_message, {Message}),
	{noreply, State}.


generate_process_name (Key, Module) when is_integer (Key) ->
	Name = erlang:list_to_atom (erlang:atom_to_list (Module) ++ "#" ++ erlang:integer_to_list (Key)),
	case erlang:whereis (Name) of
		undefined ->
			{ok, Name};
		_ ->
			generate_process_name (Key + 1, Module)
	end;
	
generate_process_name (_Key, Module) ->
	generate_process_name (1, Module).
