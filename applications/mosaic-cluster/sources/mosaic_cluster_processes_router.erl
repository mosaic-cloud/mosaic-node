
-module (mosaic_cluster_processes_router).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([resolve_alias/1, register_alias/2, unregister_alias/1]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_cluster_sup:start_child_daemon (mosaic_cluster_processes_router, {local, mosaic_process_router}, [Configuration], permanent).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_cluster_processes_router, QualifiedName, Configuration).


resolve_alias (Alias)
		when is_binary (Alias), (byte_size (Alias) > 0) ->
	gen_server:call (mosaic_process_router, {mosaic_cluster_processes_router, resolve_alias, Alias}).

register_alias (Alias, Identifier)
		when is_binary (Alias), (byte_size (Alias) > 0), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	gen_server:call (mosaic_process_router, {mosaic_cluster_processes_router, register_alias, Alias, Identifier}).

unregister_alias (Alias)
		when is_binary (Alias), (byte_size (Alias) > 0) ->
	gen_server:call (mosaic_process_router, {mosaic_cluster_processes_router, unregister_alias, Alias}).


-record (state, {qualified_name, aliases}).


init ({QualifiedName, defaults}) ->
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		State = #state{qualified_name = QualifiedName, aliases = orddict:new ()},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (_Reason, _State = #state{}) ->
	ok.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_process_router, call, Identifier, Operation, Inputs, Data}, Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (Operation), is_binary (Data) ->
	handle_cast ({mosaic_process_router, call, Identifier, Operation, Inputs, Data, Sender}, State);
	
handle_call ({mosaic_process_router, call, Identifier, Operation, Inputs, Data, FinalSender}, Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (Operation), is_binary (Data) ->
	gen_server:reply (Sender, ok),
	handle_cast ({mosaic_process_router, call, Identifier, Operation, Inputs, Data, FinalSender}, State);
	
handle_call ({mosaic_process_router, resolve, Identifier}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	try
		Process = enforce_ok_1 (execute_resolve (Identifier)),
		{reply, {ok, Process}, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, register, Identifier, Process}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	try
		Mutator = fun
						({_Identifier_, none, Processes}) ->
							{ok, none, [Process | Processes]};
						({_Identifier_}) ->
							{ok, none, [Process]}
					end,
		ok = enforce_ok (mosaic_cluster_storage:update (Identifier, Mutator)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, unregister, Identifier, Process}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	{reply, {error, unsupported_request}, State};
	
handle_call ({mosaic_cluster_processes_router, resolve_alias, Alias}, _Sender, State = #state{aliases = Aliases})
		when  is_binary (Alias), (byte_size (Alias) > 0) ->
	case orddict:find (Alias, Aliases) of
		{ok, Identifier} ->
			{reply, {ok, Identifier}, State};
		error ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({mosaic_cluster_processes_router, register_alias, Alias, Identifier}, _Sender, OldState = #state{aliases = OldAliases})
		when is_binary (Alias), (byte_size (Alias) > 0), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	try
		Registered = orddict:is_key (Alias, OldAliases),
		ok = if
			Registered -> throw ({error, already_exists});
			true -> ok
		end,
		NewAliases = orddict:store (Alias, Identifier, OldAliases),
		NewState = OldState#state{aliases = NewAliases},
		{reply, ok, NewState}
	catch throw : Error = {error, _Reason} -> {reply, Error, OldState} end;
	
handle_call ({mosaic_cluster_processes_router, unregister_alias, Alias}, _Sender, State = #state{})
		when  is_binary (Alias), (byte_size (Alias) > 0) ->
	{reply, {error, unsupported_request}, State};
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast ({mosaic_process_router, call, Identifier, Operation, Inputs, Data, Sender = {SenderProcess, SenderReference}}, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (Operation), is_binary (Data),
				is_pid (SenderProcess), is_reference (SenderReference) ->
	try
		Process = enforce_ok_1 (execute_resolve (Identifier)),
		Process ! {'$gen_call', Sender, {mosaic_process, call, Operation, Inputs, Data}},
		{noreply, State}
	catch
		throw : Error = {error, _Reason} ->
			_ = gen_server:reply (Sender, Error),
			{noreply, State}
	end;
	
handle_cast ({mosaic_process_router, cast, Identifier, Operation, Inputs, Data}, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (Operation), is_binary (Data) ->
	try
		Process = enforce_ok_1 (execute_resolve (Identifier)),
		Process ! {'$gen_cast', {mosaic_process, cast, Operation, Inputs, Data}},
		{noreply, State}
	catch throw : {error, _Reason} -> {noreply, State} end;
	
handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, State = #state{}) ->
	{stop, {error, {invalid_message, Message}}, State}.


execute_resolve (Identifier) ->
	case mosaic_cluster_processes:resolve (Identifier) of
		Outcome = {ok, _Process} ->
			Outcome;
		{error, does_not_exist} ->
			case mosaic_cluster_storage:select (Identifier) of
				{ok, none, RegisteredProcesses} ->
					LiveProcesses = lists:filter (
								fun (Process) ->
									Monitor = erlang:monitor (process, Process),
									receive
										{'DOWN', Monitor, process, Process, _} ->
											false
									after 0 ->
										true = demonitor(Monitor, [flush]),
										true
									end
								end,
								RegisteredProcesses),
					case LiveProcesses of
						[] ->
							{error, {noproc, group_is_empty}};
						_ ->
							Process = lists:nth (random:uniform (erlang:length (LiveProcesses)), LiveProcesses),
							{ok, Process}
					end;
				{ok, undefined, {mosaic_cluster_processes, definition, _, _, _}} ->
					{error, {noproc, process_does_not_exist}};
				{ok, _, _} ->
					{error, {noproc, resolve_failed}};
				{error, does_not_exist} ->
					{error, {noproc, process_does_not_exist}};
				{error, Reason} ->
					{error, {noproc, {resolve_failed, Reason}}}
			end;
		{error, Reason} ->
			{error, {noproc, {resolve_failed, Reason}}}
	end.
