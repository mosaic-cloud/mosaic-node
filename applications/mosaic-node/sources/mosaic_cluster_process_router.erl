
-module (mosaic_cluster_process_router).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_node_sup:start_child_daemon ({local, mosaic_process_router}, mosaic_cluster_process_router, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_cluster_process_router, QualifiedName, Configuration).


-record (state, {qualified_name}).


init ({QualifiedName, defaults}) ->
	try
		ok = enforce_ok (mosaic_process_tools:ensure_registered (QualifiedName)),
		State = #state{qualified_name = QualifiedName},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (_Reason, _State = #state{}) ->
	ok.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_process_router, call, Identifier, Operation, Inputs, Data}, Sender, State = #state{})
		when is_binary (Identifier), is_binary (Operation), is_binary (Data) ->
	try
		enforce_ok (execute_call (Identifier, Operation, Inputs, Data, Sender)),
		{noreply, State}
	catch throw : _Error = {error, _Reason} -> {noreply, State} end;
	
handle_call ({mosaic_process_router, call, Identifier, Operation, Inputs, Data, FinalSender}, _Sender, State = #state{})
		when is_binary (Identifier), is_binary (Operation), is_binary (Data) ->
	try
		enforce_ok (execute_call (Identifier, Operation, Inputs, Data, FinalSender)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, resolve, Identifier}, _Sender, State = #state{})
		when is_binary (Identifier) ->
	try
		Process = enforce_ok_1 (execute_resolve_all (Identifier)),
		{reply, {ok, Process}, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, register_group, Group, {}}, _Sender, State = #state{})
		when is_binary (Group), (bit_size (Group) =:= 160) ->
	try
		enforce_ok (execute_register_group (Group)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, unregister_group, Group, {}}, _Sender, State = #state{})
		when is_binary (Group), (bit_size (Group) =:= 160) ->
	try
		enforce_ok (execute_unregister_group (Group)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, register_group, Group, Process}, _Sender, State = #state{})
		when is_binary (Group), (bit_size (Group) =:= 160), is_pid (Process) ->
	try
		enforce_ok (execute_register_group (Group, Process)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, unregister_group, Group, Process}, _Sender, State = #state{})
		when is_binary (Group), (bit_size (Group) =:= 160), is_pid (Process) ->
	try
		enforce_ok (execute_unregister_group (Group, Process)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, register_alias, Alias, Identifier}, _Sender, State = #state{})
		when is_binary (Alias), (bit_size (Alias) =:= 160), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	try
		enforce_ok (execute_register_alias (Alias, Identifier)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_router, unregister_alias, Alias}, _Sender, State = #state{})
		when  is_binary (Alias), (bit_size (Alias) =:= 160) ->
	try
		enforce_ok (execute_unregister_alias (Alias)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast ({mosaic_process_router, call, Identifier, Operation, Inputs, Data, Sender}, State = #state{})
		when is_binary (Identifier), is_binary (Operation), is_binary (Data) ->
	try
		enforce_ok (execute_call (Identifier, Operation, Inputs, Data, Sender)),
		{noreply, State}
	catch throw : {error, _Reason} -> {noreply, State} end;
	
handle_cast ({mosaic_process_router, cast, Identifier, Operation, Inputs, Data}, State = #state{})
		when is_binary (Identifier), is_binary (Operation), is_binary (Data) ->
	try
		enforce_ok (execute_cast (Identifier, Operation, Inputs, Data)),
		{noreply, State}
	catch throw : {error, _Reason} -> {noreply, State} end;
	
handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, State = #state{}) ->
	{stop, {error, {invalid_message, Message}}, State}.


execute_call (Identifier, Operation, Inputs, Data, Sender) ->
	try
		Process = enforce_ok_1 (execute_resolve_all (Identifier)),
		Process ! {'$gen_call', Sender, {mosaic_process, call, Operation, Inputs, Data}},
		ok
	catch
		throw : Error = {error, _Reason} ->
			gen_server:reply (Sender, Error),
			Error
	end.


execute_cast (Identifier, Operation, Inputs, Data) ->
	Process = enforce_ok_1 (execute_resolve_all (Identifier)),
	Process ! {'$gen_cast', {mosaic_process, cast, Operation, Inputs, Data}},
	ok.


execute_resolve_all (Name)
		when is_binary (Name), (bit_size (Name) =:= 160) ->
	execute_resolve_all ({process, Name});
	
execute_resolve_all ({process, Identifier}) ->
	case mosaic_cluster_processes:resolve (Identifier) of
		Outcome = {ok, _Process} ->
			Outcome;
		{error, does_not_exist} ->
			execute_resolve_all ({group, Identifier});
		{error, Reason} ->
			{error, {noproc, Reason}}
	end;
	
execute_resolve_all ({group, Identifier}) ->
	case execute_resolve_group (Identifier) of
		Outcome = {ok, _Process} ->
			Outcome;
		{error, group_not_registered} ->
			execute_resolve_all ({alias, Identifier});
		{error, Reason} ->
			{error, {noproc, Reason}}
	end;
	
execute_resolve_all ({alias, Alias}) ->
	case execute_resolve_alias (Alias) of
		{ok, Identifier} ->
			% FIXME: Possible infinite recursion starting from here...
			execute_resolve_all ({process, Identifier});
		{error, alias_not_registered} ->
			{error, noproc};
		{error, Reason} ->
			{error, {noproc, Reason}}
	end.


execute_resolve_group (Group) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, group, Group})),
	case mosaic_cluster_storage:select (Key) of
		{ok, undefined, {mosaic_cluster_processes, group, Group, Processes}} ->
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
						Processes),
			case LiveProcesses of
				[] ->
					{error, group_is_empty};
				_ ->
					Process = lists:nth (random:uniform (erlang:length (LiveProcesses)), LiveProcesses),
					{ok, Process}
			end;
		{ok, _, _} ->
			{error, group_not_registered};
		{error, does_not_exist} ->
			{error, group_not_registered};
		Error = {error, _Reason} ->
			Error
	end.


execute_register_group (Group) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, group, Group})),
	Mutator = fun
			({Key_, undefined, {mosaic_cluster_processes, group, Group_, Processes}}) when (Key_ =:= Key), (Group_ =:= Group) ->
				{ok, undefined, {mosaic_cluster_processes, group, Group, Processes}};
			({Key_}) when (Key_ =:= Key) ->
				{ok, undefined, {mosaic_cluster_processes, group, Group, []}}
	end,
	enforce_ok (mosaic_cluster_storage:update (Key, Mutator)),
	ok.


execute_unregister_group (Group) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, group, Group})),
	enforce_ok (mosaic_cluster_storage:exculde (Key, undefined)),
	ok.


execute_register_group (Group, Process) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, group, Group})),
	Mutator = fun
			({Key_, undefined, {mosaic_cluster_processes, group, Group_, Processes}}) when (Key_ =:= Key), (Group_ =:= Group) ->
				{ok, undefined, {mosaic_cluster_processes, group, Group, [Process | Processes]}};
			({Key_}) when (Key_ =:= Key) ->
				{ok, undefined, {mosaic_cluster_processes, group, Group, [Process]}}
	end,
	enforce_ok (mosaic_cluster_storage:update (Key, Mutator)),
	ok.


execute_unregister_group (Group, Process) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, group, Group})),
	Mutator = fun
			({Key_, undefined, {mosaic_cluster_processes, group, Group_, Processes}}) when (Key_ =:= Key), (Group_ =:= Group) ->
				{ok, undefined, {mosaic_cluster_processes, group, Group, lists:delete (Process, Processes)}};
			({Key_}) when (Key_ =:= Key) ->
				{ok, undefined, {mosaic_cluster_processes, group, Group, []}}
	end,
	enforce_ok (mosaic_cluster_storage:update (Key, Mutator)),
	ok.


execute_resolve_alias (Alias) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, alias, Alias})),
	case mosaic_cluster_storage:select (Key) of
		{ok, undefined, {mosaic_cluster_processes, alias, Alias, Identifier}} ->
			{ok, Identifier};
		{ok, _, _} ->
			{error, alias_not_registered};
		{error, does_not_exist} ->
			{error, alias_not_registered};
		Error = {error, _Reason} ->
			Error
	end.


execute_register_alias (Alias, Identifier) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, alias, Alias})),
	enforce_ok (mosaic_cluster_storage:include (Key, undefined, {mosaic_cluster_processes, alias, Alias, Identifier})),
	ok.


execute_unregister_alias (Alias) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, alias, Alias})),
	enforce_ok (mosaic_cluster_storage:exculde (Key, undefined)),
	ok.
