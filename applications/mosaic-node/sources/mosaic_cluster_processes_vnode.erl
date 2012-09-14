
-module (mosaic_cluster_processes_vnode).

-behaviour (riak_core_vnode).


-export ([
		start_vnode/1, init/1, terminate/2, delete/1,
		handle_command/3, is_empty/1,
		handoff_starting/2, handoff_finished/2, handoff_cancelled/1,
		handle_handoff_command/3, handle_handoff_data/2,
		encode_handoff_item/2]).


-record (state, {qualified_name, partition, process_controller}).


start_vnode (Partition) ->
	riak_core_vnode_master:get_vnode_pid (Partition, mosaic_cluster_processes_vnode).


init ([Partition]) ->
	true = erlang:process_flag (trap_exit, true),
	{ok, VnodeLocalName} = mosaic_cluster_tools:service_process_name (mosaic_cluster_processes, vnode, Partition),
	{ok, ProcessControllerLocalName} = mosaic_cluster_tools:service_process_name (mosaic_cluster_processes, process_controller, Partition),
	VnodeQualifiedName = {local, VnodeLocalName},
	ProcessControllerQualifiedName = {local, ProcessControllerLocalName},
	case mosaic_process_tools:ensure_registered (VnodeQualifiedName, erlang:self ()) of
		ok ->
			case mosaic_process_controller:start_supervised (ProcessControllerQualifiedName, defaults) of
				{ok, ProcessController} ->
					true = erlang:link (ProcessController),
					{ok, #state{qualified_name = VnodeQualifiedName, partition = Partition, process_controller = ProcessController}};
				{error, Reason} ->
					{stop, {failed_starting_subordinate, process_controller, Reason}}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{process_controller = ProcessController}) ->
	ok = mosaic_process_controller:stop (ProcessController, normal),
	ok.


delete (State = #state{}) ->
	{ok, State}.


is_empty (State = #state{process_controller = ProcessController}) ->
	{ok, ProcessCount} = mosaic_process_controller:count (ProcessController),
	{ProcessCount =:= 0, State}.


handle_command ({mosaic_cluster, ping, Key}, _Sender, State = #state{partition = Partition})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	{reply, {pong, Key, erlang:self (), mosaic_cluster_processes, {Partition, erlang:node ()}}, State};
	
handle_command ({mosaic_cluster_processes, resolve, Key}, _Sender, State = #state{process_controller = ProcessController})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_process_controller:resolve (ProcessController, Key) of
		Outcome = {ok, _Process} ->
			{reply, Outcome, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_processes, examine, Key}, _Sender, State = #state{})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_cluster_storage:select (Key) of
		{ok, undefined, {mosaic_cluster_processes, definition, Type, ConfigurationEncoding, ConfigurationContent}} ->
			Details = [
					{type, Type},
					{configuration, ConfigurationEncoding, ConfigurationContent}],
			{reply, {ok, Details}, State};
		{ok, _, _} ->
			{reply, {error, invalid_key}, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_processes, define, Key, Type, ConfigurationEncoding, ConfigurationContent}, _Sender, State = #state{})
		when is_binary (Key), (bit_size (Key) =:= 160), is_atom (Type), is_atom (ConfigurationEncoding) ->
	case mosaic_cluster_storage:include (Key, undefined, {mosaic_cluster_processes, definition, Type, ConfigurationEncoding, ConfigurationContent}) of
		ok ->
			{reply, ok, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_processes, create, Key}, _Sender, State = #state{process_controller = ProcessController})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_cluster_storage:select (Key) of
		{ok, undefined, {mosaic_cluster_processes, definition, Type, ConfigurationEncoding, ConfigurationContent}} ->
			case mosaic_process_configurator:configure (Type, create, Key, ConfigurationEncoding, ConfigurationContent) of
				{ok, Module, Configuration} ->
					case mosaic_process_controller:create (ProcessController, Key, Module, Configuration) of
						{ok, _Process} ->
							{reply, ok, State};
						Error = {error, _Reason} ->
							{reply, Error, State}
					end;
				Error = {error, _Reason} ->
					{reply, Error, State}
			end;
		{ok, _, _} ->
			{reply, {error, invalid_storage}, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_processes, stop, Key, Signal}, _Sender, State = #state{process_controller = ProcessController})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_process_controller:resolve (ProcessController, Key) of
		{ok, Process} ->
			case mosaic_process:stop (Process, Signal) of
				ok ->
					{reply, ok, State};
				Error = {error, _Reason} ->
					{reply, Error, State}
			end;
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster, list}, _Sender, State = #state{process_controller = ProcessController}) ->
	{ok, Keys} = mosaic_process_controller:fold (ProcessController,
			fun ({Key, _Process}, Keys) ->
				[Key | Keys]
			end, []),
	{reply, {ok, Keys}, State};
	
handle_command ({mosaic_cluster, map, Mapper}, _Sender, State = #state{})
		when is_function (Mapper, 2) ->
	{reply, {error, unsupported_request}, State};
	
handle_command (Request, Sender, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid command request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handoff_starting (_Node, State = #state{}) ->
	{true, State}.


handoff_finished (_Node, State = #state{}) ->
	is_empty (State).


handoff_cancelled (State = #state{}) ->
	{ok, State}.


handle_handoff_command ({riak_core_fold_req_v1, Function, InputAccumulator}, _Sender, State = #state{process_controller = ProcessController}) ->
	Self = erlang:self (),
	{ok, OutputAccumulator} = mosaic_process_controller:fold (ProcessController,
			fun ({Key, Process}, CurrentAccumulator) when is_binary (Key), (bit_size (Key) =:= 160), is_pid (Process) ->
				Function (Key, {mosaic_cluster_processes, Self, process, Key}, CurrentAccumulator)
			end, InputAccumulator),
	{reply, OutputAccumulator, State};
	
handle_handoff_command ({mosaic_cluster_processes, handoff_request, Vnode, Reference, process, Key, PeerProcessController}, _Sender, State = #state{process_controller = ProcessController})
		when is_pid (Vnode), is_reference (Reference), is_binary (Key), (bit_size (Key) =:= 160), is_pid (PeerProcessController) ->
	% ok = mosaic_transcript:trace_information ("requested process handoff as source...", [{vnode, Vnode}, {key, Key}]),
	case mosaic_cluster_storage:select (Key) of
		{ok, undefined, {mosaic_cluster_processes, definition, Type, _, _}} ->
			case mosaic_process_configurator:configure (Type, {migrate, source}, Key, term, defaults) of
				{ok, none, SourceConfiguration} ->
					case mosaic_process_configurator:configure (Type, {migrate, target}, Key, term, defaults) of
						{ok, TargetModule, TargetConfiguration} ->
							case mosaic_process_controller:migrate (ProcessController, PeerProcessController, Key, SourceConfiguration, TargetModule, TargetConfiguration) of
								ok ->
									% ok = mosaic_transcript:trace_information ("succeeded process handoff as source;", [{vnode, Vnode}, {key, Key}]),
									Vnode ! {mosaic_cluster_processes, handoff_request, Reference, succeeded},
									{reply, ok, State};
								Error = {error, Reason} ->
									ok = mosaic_transcript:trace_error ("failed process handoff as source; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
									Vnode ! {mosaic_cluster_processes, handoff_request, Reference, failed, Reason},
									{reply, Error, State}
							end;
						Error = {error, Reason} ->
							ok = mosaic_transcript:trace_error ("failed process handoff as source; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
							Vnode ! {mosaic_cluster_processes, handoff_request, Reference, failed, Reason},
							{reply, Error, State}
					end;
				Error = {error, Reason} ->
					ok = mosaic_transcript:trace_error ("failed process handoff as source; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
					Vnode ! {mosaic_cluster_processes, handoff_request, Reference, failed, Reason},
					{reply, Error, State}
			end;
		{ok, _, _} ->
			Reason = invalid_storage,
			ok = mosaic_transcript:trace_error ("failed process handoff as source; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
			Vnode ! {mosaic_cluster_processes, handoff_request, Reference, failed, Reason},
			{reply, {error, Reason}, State};
		Error = {error, Reason} ->
			ok = mosaic_transcript:trace_error ("failed process handoff as source; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
			Vnode ! {mosaic_cluster_processes, handoff_request, Reference, failed, Reason},
			{reply, Error, State}
	end;
	
handle_handoff_command (Request, Sender, State = #state{}) ->
	ok = mosaic_transcript:trace_warning ("received unknown command request during handoff; forwarding!", [{request, Request}, {sender, Sender}]),
	{forward, State}.


handle_handoff_data (DataBinary, State = #state{process_controller = ProcessController}) ->
	DataTerm = try
		erlang:binary_to_term (DataBinary)
	catch
		error : badarg ->
			{error, {invalid_handoff_data, DataBinary}}
	end,
	case DataTerm of
		{mosaic_cluster_processes, Vnode, process, Key} when is_pid (Vnode), is_binary (Key), (bit_size (Key) =:= 160) ->
			Reference = erlang:make_ref (),
			% ok = mosaic_transcript:trace_information ("requesting process handoff as target...", [{vnode, Vnode}, {key, Key}]),
			ok = riak_core_vnode:send_command (Vnode, {mosaic_cluster_processes, handoff_request, erlang:self (), Reference, process, Key, ProcessController}),
			receive
				{mosaic_cluster_processes, handoff_request, Reference, succeeded} ->
					% ok = mosaic_transcript:trace_information ("succeeded process handoff as target;", [{vnode, Vnode}, {key, Key}]),
					{reply, ok, State};
				{mosaic_cluster_processes, handoff_request, Reference, failed, Reason} ->
					ok = mosaic_transcript:trace_error ("failed process handoff as target; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
					{reply, {error, Reason}, State}
			end;
		Error = {error, Reason} ->
			ok = mosaic_transcript:trace_error ("received invalid handoff data", [{data, DataBinary}, {reason, Reason}]),
			{reply, Error, State};
		_ ->
			ok = mosaic_transcript:trace_error ("received invalid handoff data", [{data, DataBinary}]),
			{reply, {error, {invalid_handoff_data, DataTerm}}, State}
	end.


encode_handoff_item (Key, DataTerm = {mosaic_cluster_processes, Vnode, process, Key})
		when is_pid (Vnode), is_binary (Key), (bit_size (Key) =:= 160) ->
	erlang:term_to_binary (DataTerm).
