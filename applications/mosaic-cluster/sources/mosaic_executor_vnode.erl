
-module (mosaic_executor_vnode).

-behaviour (riak_core_vnode).

-export ([service_activate/0, service_deactivate/0]).
-export ([
		start_vnode/1, init/1, terminate/2, delete/1,
		handle_command/3, is_empty/1,
		handoff_starting/2, handoff_finished/2, handoff_cancelled/1,
		handle_handoff_command/3, handle_handoff_data/2,
		encode_handoff_item/2]).

-record (state, {qualified_name, partition, object_store, process_controller}).

service_activate () ->
	case mosaic_cluster_sup:start_child_vnode_master (mosaic_executor_vnode) of
		{ok, Master} when is_pid (Master) ->
			ok = riak_core:register_vnode_module (mosaic_executor_vnode),
			ok = riak_core_node_watcher:service_up (mosaic_executor, Master),
			ok;
		{error, already_started} ->
			MasterName = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
			case erlang:whereis (MasterName) of
				Master when is_pid (Master) ->
					ok = riak_core_node_watcher:service_up (mosaic_executor, Master),
					ok;
				undefined ->
					{error, vnode_master_not_started}
			end;
		Error = {error, _Reason} ->
			Error
	end.

service_deactivate () ->
	case riak_core_node_watcher:service_down (mosaic_executor) of
		ok ->
			ok;
		Error = {error, _Reason} ->
			Error
	end.

start_vnode (Partition) ->
	riak_core_vnode_master:get_vnode_pid (Partition, mosaic_executor_vnode).

init ([Partition]) ->
	true = erlang:process_flag (trap_exit, true),
	QualifiedName = {local, generate_local_name (Partition)},
	ObjectStoreQualifiedName = {local, generate_object_store_local_name (Partition)},
	ProcessControllerQualifiedName = {local, generate_process_controller_local_name (Partition)},
	case mosaic_tools:ensure_registered (QualifiedName, erlang:self ()) of
		ok ->
			case mosaic_object_store:start_supervised (ObjectStoreQualifiedName, defaults) of
				{ok, ObjectStore} ->
					true = erlang:link (ObjectStore),
					case mosaic_process_controller:start_supervised (ProcessControllerQualifiedName, defaults) of
						{ok, ProcessController} ->
							true = erlang:link (ProcessController),
							{ok, #state{
									qualified_name = QualifiedName, partition = Partition,
									object_store = ObjectStore, process_controller = ProcessController}};
						{error, Reason} ->
							{stop, {failed_starting_subordinate, {process_controller, ProcessControllerQualifiedName, Reason}}}
					end;
				{error, Reason} ->
					{stop, {failed_starting_subordinate, {object_store, ObjectStoreQualifiedName, Reason}}}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.

terminate (Signal, _State = #state{object_store = ObjectStore, process_controller = ProcessController}) ->
	Outcome1 = mosaic_process_controller:stop (ProcessController, Signal),
	Outcome2 = mosaic_object_store:stop (ObjectStore, Signal),
	case {Outcome1, Outcome2} of
		{ok, ok} ->
			ok;
		{{error, Reason}, ok} ->
			{error, {failed_stopping_subordinates, [{process_controller, Reason}]}};
		{ok, {error, Reason}} ->
			{error, {failed_stopping_subordinates, [{object_store, Reason}]}};
		{{error, Reason1}, {error, Reason2}} ->
			{error, {failed_stopping_subordinates, [{process_controller, Reason1}, {object_store, Reason2}]}}
	end.

delete (State) ->
	{ok, State}.

is_empty (State = #state{object_store = ObjectStore, process_controller = ProcessController}) ->
	{ok, ObjectCount} = mosaic_object_store:count (ObjectStore),
	{ok, ProcessCount} = mosaic_process_controller:count (ProcessController),
	{(ObjectCount + ProcessCount) == 0, State}.

handle_command ({ping, Key}, _Sender, State) ->
	{reply, {pong, Key, {State#state.partition, erlang:node ()}}, State};
	
handle_command ({define_process, Key, Module, Arguments}, _Sender, State = #state{object_store = ObjectStore})
		when is_atom (Module) ->
	ok = mosaic_object_store:include (ObjectStore, Key, none, {Module, Arguments}),
	{reply, {ok, Key}, State};
	
handle_command ({create_process, Key}, _Sender, State = #state{object_store = ObjectStore, process_controller = ProcessController}) ->
	{ok, {Key, none, {Module, Arguments}}} = mosaic_object_store:select (ObjectStore, Key),
	{ok, Process} = mosaic_process_controller:create (ProcessController, Key, Module, Arguments),
	{reply, {ok, Process}, State};
	
handle_command ({stop_process, Key, Signal}, _Sender, State = #state{process_controller = ProcessController}) ->
	case mosaic_process_controller:resolve (ProcessController, Key) of
		{ok, Process} when is_pid (Process) ->
			case mosaic_process:stop (Process, Signal) of
				ok ->
					{reply, ok, State};
				Error = {error, _Reason} ->
					{reply, Error, State}
			end;
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({riak_core_fold_req_v1, Fun, InputAcc}, _Sender, State = #state{object_store = ObjectStore, process_controller = ProcessController}) ->
	{ok, OutputAcc1} = mosaic_object_store:fold (ObjectStore,
			fun ({Key, none, Object}, CurrentAcc) ->
				Fun (Key, {object, ObjectStore, Object}, CurrentAcc)
			end, InputAcc),
	{ok, OutputAcc2} = mosaic_process_controller:fold (ProcessController,
			fun ({Key, Process}, CurrentAcc) ->
				Fun (Key, {process, ProcessController, Process}, CurrentAcc)
			end, OutputAcc1),
	{reply, OutputAcc2, State};
	
handle_command (Command, _Sender, State) ->
	{reply, {error, {invalid_command, Command}}, State}.

handoff_starting (_Node, State) ->
	{true, State}.

handoff_finished (Node, State = #state{process_controller = ProcessController}) ->
	{ok, ProcessCount} = mosaic_process_controller:count (ProcessController),
	if
		(ProcessCount == 0) ->
			ok;
		(ProcessCount > 0) ->
			ok = timer:sleep (1000),
			handoff_finished (Node, State)
	end.

handoff_cancelled (State) ->
	{ok, State}.

handle_handoff_command (Request = {riak_core_fold_req_v1, _Fun, _Acc}, Sender, State) ->
	handle_command (Request, Sender, State);
	
handle_handoff_command (_Request, _Sender, State) ->
	{forward, State}.

handle_handoff_data (DataBinary, State = #state{object_store = ObjectStore, process_controller = ProcessController}) ->
	DataTerm = erlang:binary_to_term (DataBinary),
	%ok = mosaic_tools:report_info (mosaic_executor_vnode, handle_handoff_data, data_term, DataTerm),
	ok = case DataTerm of
		{Key, {object, _PeerObjectStore, Object}} ->
			ok = mosaic_object_store:include (ObjectStore, Key, none, Object),
			ok;
		{Key, {process, PeerProcessController, _PeerProcess}} ->
			{ok, _Target} = mosaic_process_controller:migrate (PeerProcessController, ProcessController, Key),
			ok
	end,
	{reply, ok, State}.

encode_handoff_item (Key, Object) ->
	erlang:term_to_binary ({Key, Object}).

generate_local_name (Partition) ->
	erlang:list_to_atom (erlang:atom_to_list (mosaic_executor_vnode) ++ "#" ++ generate_partition_prefix (Partition)).

generate_object_store_local_name (Partition) ->
	erlang:list_to_atom ("mosaic_executor_vnode#object_store#" ++ generate_partition_prefix (Partition)).

generate_process_controller_local_name (Partition) ->
	erlang:list_to_atom ("mosaic_executor_vnode#process_controller#" ++ generate_partition_prefix (Partition)).

generate_partition_prefix (Partition) when is_number (Partition), Partition >= 0, Partition < 1461501637330902918203684832716283019655932542976 ->
	IdentifierHex = string:to_lower (erlang:integer_to_list (Partition, 16)),
	IdentifierHexPadded = lists:duplicate (40 - erlang:length (IdentifierHex), $0) ++ IdentifierHex,
	IdentifierHexTrimmed = string:sub_string (IdentifierHexPadded, 1, 8),
	IdentifierHexTrimmed.
