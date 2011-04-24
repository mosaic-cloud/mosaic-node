
-module (mosaic_port_process).

-behaviour (mosaic_process).

-export ([
		init/1, terminate/2, handle_stop/2,
		handle_call/3, handle_cast/2, handle_info/2,
		begin_migration/2, commit_migration/1, rollback_migration/1]).

-record (state, {status, arguments, port_name, port_settings, port}).

init ({create, Arguments}) ->
	case initialize (Arguments, #state{status = created}) of
		{ok, State1} ->
			case create_port (State1) of
				{ok, State2} ->
					{ok, State2};
				{error, Reason, _State2} ->
					{stop, Reason}
			end;
		{error, Reason, _State1} ->
			{stop, Reason}
	end;
	
init (migrate) ->
	{ok, #state{status = pre_migrating_as_target}};
	
init (_Disposition) ->
	{stop, invalid_disposition}.

terminate (_Reason, OldState = #state{status = running}) ->
	case destroy_port (OldState) of
		{ok, _NewState} ->
			ok;
		{error, _Reason, _NewState} ->
			ok
	end;
	
terminate (_Reason, _State = #state{status = Status, port = undefined})
		when (Status =:= closed) or (Status =:= failed)
			or (Status =:= pre_migrating_as_target) or (Status =:= migration_succeeded) or (Status =:= migration_failed) ->
	ok.

handle_stop (Signal, State = #state{status = running}) ->
	case Signal of
		normal ->
			{stop, normal, ok, State};
		_ ->
			{reply, {error, invalid_signal}, State}
	end;
	
handle_stop (_Signal, State = #state{status = Status})
		when (Status =:= pre_migrating_as_target) or (Status =:= migrating_as_source) or (Status =:= migrating_as_target) ->
	{reply, {error, invalid_state}, State}.

handle_call (_Request, _Sender, State = #state{status = running}) ->
	{reply, {error, invalid_request}, State};
	
handle_call (_Request, _Sender, State = #state{status = Status})
		when (Status =:= pre_migrating_as_target) or (Status =:= migrating_as_source) or (Status =:= migrating_as_target) ->
	{reply, {error, invalid_state}, State}.

handle_cast (Request, State = #state{status = running}) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_port_process, handle_cast, erlang:self ()}, {invalid_request, Request}]),
	{noreply, State};
	
handle_cast (_Request, State = #state{status = Status})
		when (Status =:= pre_migrating_as_target) or (Status =:= migrating_as_source) or (Status =:= migrating_as_target) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_port_process, handle_cast, erlang:self ()}, invalid_status]),
	{noreply, State}.

handle_info (Message, State = #state{status = running}) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_port_process, handle_info, erlang:self ()}, {invalid_info_message, Message}]),
	{noreply, State};
	
handle_info (_Message, State = #state{status = Status})
		when (Status =:= pre_migrating_as_target) or (Status =:= migrating_as_source) or (Status =:= migrating_as_target) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_port_process, handle_cast, erlang:self ()}, invalid_status]),
	{noreply, State}.

begin_migration ({source, defaults, CompletionFun}, OldState = #state{status = running, arguments = CreationArguments}) ->
	ok = CompletionFun ({prepared, CreationArguments}),
	ok = CompletionFun (completed),
	{continue, OldState#state{status = migrating_as_source}};
	
begin_migration ({source, _MigrationArguments, _CompletionFun}, State = #state{status = running}) ->
	{reject, invalid_arguments, State};
	
begin_migration ({target, Arguments, CompletionFun}, OldState = #state{status = pre_migrating_as_target}) ->
	case initialize (Arguments, OldState) of
		{ok, NewState} ->
			ok = CompletionFun (completed),
			{continue, NewState#state{status = migrating_as_target}};
		{error, Reason, NewState} ->
			{terminate, Reason, NewState#state{status = migration_failed}}
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
	
rollback_migration (OldState = #state{status = Status}) when (Status =:= pre_migrating_as_target) or (Status =:= migrating_as_target) ->
	{continue, OldState#state{status = migration_failed}}.

initialize (
		Arguments,
		OldState = #state{status = Status, arguments = undefined, port_name = undefined, port_settings = undefined, port = undefined})
		when (Status =:= created) or (Status =:= pre_migrating_as_target) ->
	case Arguments of
		{PortName = {spawn_executable, PortExecutable}, PortSettings} when is_list (PortExecutable), is_list (PortSettings) ->
			{ok, OldState#state{arguments = Arguments, port_name = PortName, port_settings = PortSettings}};
		_ ->
			{error, invalid_arguments, OldState}
	end.

create_port (OldState = #state{status = Status, port = undefined, port_name = PortName, port_settings = PortSettings})
		when (Status =:= created) or (Status =:= migrating_as_target) ->
	Port = erlang:open_port (PortName, PortSettings),
	true = erlang:port_connect (Port, erlang:self ()),
	{ok, OldState#state{status = running, port = Port}}.

destroy_port (OldState = #state{status = Status, port = Port})
		when (Status =:= running) or (Status =:= migrating_as_source) ->
	true = erlang:port_close (Port),
	true = erlang:unlink (Port),
	{ok, OldState#state{status = closed, port = undefined}}.
