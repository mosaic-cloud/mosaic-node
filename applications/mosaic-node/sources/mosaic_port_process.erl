
-module (mosaic_port_process).

-behaviour (mosaic_process).


-export ([
		init/3, terminate/2, handle_stop/2,
		handle_call/5, handle_cast/4, handle_info/2,
		begin_migration/4, commit_migration/1, rollback_migration/1]).


-record (state, {status, configuration, port}).


init (create, _Identifier, Configuration) ->
	case validate_configuration (create, Configuration) of
		ok ->
			State1 = #state{status = created, configuration = Configuration},
			case create_port (State1) of
				{ok, State2} ->
					{ok, State2};
				{error, Reason, _State2} ->
					{stop, Reason}
			end;
		{error, Reason} ->
			{stop, Reason}
	end;
	
init (migrate, _Identifier, Configuration) ->
	case validate_configuration (migrate, Configuration) of
		ok ->
			defaults = Configuration,
			State = #state{status = pre_migrating_as_target},
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (Reason, OldState = #state{port = Port})
		when (Port =/= undefined) ->
	case destroy_port (OldState) of
		{ok, NewState} ->
			terminate (Reason, NewState);
		{error, _Reason, NewState} ->
			terminate (Reason, NewState)
	end;
	
terminate (_Reason, _State = #state{status = Status})
		when ((Status =:= closed) orelse (Status =:= pre_migrating_as_target) orelse (Status =:= migration_succeeded) orelse (Status =:= migration_failed)) ->
	ok.


handle_stop (normal, State = #state{status = running}) ->
	{stop, normal, ok, State};
	
handle_stop (Signal, State = #state{status = running}) ->
	Error = {error, {invalid_signal, Signal}},
	{stop, Error, Error, State};
	
handle_stop (_Signal, State = #state{status = Status})
		when ((Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	Error = {error, {invalid_status, Status}},
	{stop, Error, Error, State}.


handle_call (Operation, Inputs, _Data, _Sender, State = #state{status = running}) ->
	Error = {error, {invalid_request, Operation, Inputs}},
	{stop, Error, Error, State};
	
handle_call (_Operation, _Inputs, _Data, _Sender, State = #state{status = Status})
		when ((Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	Error = {error, {invalid_status, Status}},
	{stop, Error, Error, State}.


handle_cast (Operation, Inputs, _Data, State = #state{status = running}) ->
	{stop, {error, {invalid_request, Operation, Inputs}}, State};
	
handle_cast (_Operation, _Inputs, _Data, State = #state{status = Status})
		when ((Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	{stop, {error, {invalid_status, Status}}, State}.


handle_info (Message, State = #state{status = running}) ->
	{stop, {error, {invalid_message, Message}}, State};
	
handle_info (_Message, State = #state{status = Status})
		when ((Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_source) orelse (Status =:= migrating_as_target)) ->
	{stop, {error, {invalid_status, Status}}, State}.


begin_migration (source, MigrateConfiguration, CompletionFunction, OldState = #state{status = running, configuration = CreateConfiguration}) ->
	case validate_configuration (migrate, MigrateConfiguration) of
		ok ->
			defaults = MigrateConfiguration,
			ok = CompletionFunction ({prepared, CreateConfiguration}),
			ok = CompletionFunction (completed),
			{continue, OldState#state{status = migrating_as_source}};
		{error, Reason} ->
			{reject, Reason, OldState}
	end;
	
begin_migration (target, CreateConfiguration, CompletionFunction, OldState = #state{status = pre_migrating_as_target}) ->
	case validate_configuration (create, CreateConfiguration) of
		ok ->
			NewState = #state{status = migrating_as_target, configuration = CreateConfiguration},
			ok = CompletionFunction (completed),
			{continue, NewState};
		{error, Reason} ->
			{reject, Reason, OldState}
	end.


commit_migration (OldState = #state{status = migrating_as_source}) ->
	case destroy_port (OldState) of
		{ok, NewState} ->
			{continue, NewState#state{status = migration_succeeded}};
		{error, Reason, NewState} ->
			{terminate, Reason, NewState#state{status = migration_failed}}
	end;
	
commit_migration (OldState = #state{status = migrating_as_target}) ->
	case create_port (OldState) of
		{ok, NewState} ->
			{continue, NewState};
		{error, Reason, NewState} ->
			{terminate, Reason, NewState#state{status = migration_failed}}
	end.


rollback_migration (OldState = #state{status = migrating_as_source}) ->
	{continue, OldState#state{status = running}};
	
rollback_migration (OldState = #state{status = Status})
		when ((Status =:= pre_migrating_as_target) orelse (Status =:= migrating_as_target)) ->
	{continue, OldState#state{status = migration_failed}}.


create_port (OldState = #state{status = Status, configuration = Configuration})
		when ((Status =:= created) orelse (Status =:= migrating_as_target)) ->
	{{spawn_executable, Executable}, [{arg0, Arg0}]} = Configuration,
	PortName = {spawn_executable, erlang:binary_to_list (Executable)},
	PortSettings = [{arg0, erlang:binary_to_list (Arg0)}],
	Port = erlang:open_port (PortName, PortSettings),
	true = erlang:port_connect (Port, erlang:self ()),
	{ok, OldState#state{status = running, port = Port}}.


destroy_port (OldState = #state{status = Status, port = Port})
		when ((Status =:= running) orelse (Status =:= migrating_as_source)) ->
	true = erlang:port_close (Port),
	true = erlang:unlink (Port),
	{ok, OldState#state{status = closed, port = undefined}}.


validate_configuration (create, {PortName, PortSettings}) ->
	try
		ok = case PortName of
			{spawn_executable, Executable} when is_binary (Executable) ->
				ok;
			{spawn_executable, Executable} ->
				throw ({error, {invalid_executable, Executable}});
			_ ->
				throw ({error, {invalid_port_name, PortName}})
		end,
		ok = case PortSettings of
			[{arg0, Arg0}] when is_binary (Arg0) ->
				ok;
			[{arg0, Arg0}] ->
				throw ({error, {invalid_arg0, Arg0}});
			_ ->
				throw ({error, {invalid_port_settings, PortSettings}})
		end,
		ok
	catch
		throw : Error = {error, _Reason} ->
			Error
	end;
	
validate_configuration (migrate, defaults) ->
	ok;
	
validate_configuration (Disposition, Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	{error, {invalid_configuration, Configuration}}.
