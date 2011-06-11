
-module (mosaic_cluster_storage_vnode).

-behaviour (riak_core_vnode).


-export ([
		start_vnode/1, init/1, terminate/2, delete/1,
		handle_command/3, is_empty/1,
		handoff_starting/2, handoff_finished/2, handoff_cancelled/1,
		handle_handoff_command/3, handle_handoff_data/2,
		encode_handoff_item/2]).


-record (state, {qualified_name, partition, object_store}).


start_vnode (Partition) ->
	riak_core_vnode_master:get_vnode_pid (Partition, mosaic_cluster_storage_vnode).


init ([Partition]) ->
	true = erlang:process_flag (trap_exit, true),
	{ok, VnodeLocalName} = mosaic_cluster_tools:service_process_name (mosaic_cluster_storage, vnode, Partition),
	{ok, ObjectStoreLocalName} = mosaic_cluster_tools:service_process_name (mosaic_cluster_storage, object_store, Partition),
	VnodeQualifiedName = {local, VnodeLocalName},
	ObjectStoreQualifiedName = {local, ObjectStoreLocalName},
	case mosaic_process_tools:ensure_registered (VnodeQualifiedName, erlang:self ()) of
		ok ->
			case mosaic_object_store:start_supervised (ObjectStoreQualifiedName, defaults) of
				{ok, ObjectStore} ->
					true = erlang:link (ObjectStore),
					{ok, #state{qualified_name = VnodeQualifiedName, partition = Partition, object_store = ObjectStore}};
				{error, Reason} ->
					{stop, {failed_starting_subordinate, object_store, Reason}}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{object_store = ObjectStore}) ->
	ok = mosaic_object_store:stop (ObjectStore, normal),
	ok.


delete (State = #state{}) ->
	{ok, State}.


is_empty (State = #state{object_store = ObjectStore}) ->
	{ok, ObjectCount} = mosaic_object_store:count (ObjectStore),
	{ObjectCount =:= 0, State}.


handle_command ({mosaic_cluster, ping, Key}, _Sender, State = #state{partition = Partition})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	{reply, {pong, Key, erlang:self (), mosaic_cluster_storage, {Partition, erlang:node ()}}, State};
	
handle_command ({mosaic_cluster_storage, select, Key}, _Sender, State = #state{object_store = ObjectStore})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_object_store:select (ObjectStore, Key) of
		{ok, {Key, Revision, Data}} ->
			{reply, {ok, Revision, Data}, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_storage, include, Key, Revision, Data}, _Sender, State = #state{object_store = ObjectStore})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_object_store:include (ObjectStore, Key, Revision, Data) of
		ok ->
			{reply, ok, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_storage, exclude, Key, Revision, Data}, _Sender, State = #state{object_store = ObjectStore})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_object_store:exclude (ObjectStore, Key, Revision, Data) of
		ok ->
			{reply, ok, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_storage, update, Key, OldRevision, NewRevision, NewData}, _Sender, State = #state{object_store = ObjectStore})
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case mosaic_object_store:update (ObjectStore, Key, OldRevision, NewRevision, NewData) of
		ok ->
			{reply, ok, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster_storage, update, Key, Mutator}, _Sender, State = #state{object_store = ObjectStore})
		when is_binary (Key), (bit_size (Key) =:= 160), is_function (Mutator, 1) ->
	case mosaic_object_store:update (ObjectStore, Key, Mutator) of
		ok ->
			{reply, ok, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_command ({mosaic_cluster, list}, _Sender, State = #state{object_store = ObjectStore}) ->
	{ok, Keys} = mosaic_object_store:fold (ObjectStore,
			fun ({Key, _Revision, _Data}, Keys) ->
				[Key | Keys]
			end, []),
	{reply, {ok, Keys}, State};
	
handle_command (Request, Sender, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid command request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handoff_starting (_Node, State = #state{}) ->
	{true, State}.


handoff_finished (_Node, State = #state{}) ->
	is_empty (State).


handoff_cancelled (State = #state{}) ->
	{ok, State}.


handle_handoff_command ({riak_core_fold_req_v1, Function, InputAccumulator}, _Sender, State = #state{object_store = ObjectStore}) ->
	Self = erlang:self (),
	{ok, OutputAccumulator} = mosaic_object_store:fold (ObjectStore,
			fun ({Key, _Revision, _Data}, CurrentAccumulator) when is_binary (Key), (bit_size (Key) =:= 160) ->
				Function (Key, {mosaic_cluster_storage, Self, object, Key}, CurrentAccumulator)
			end, InputAccumulator),
	{reply, OutputAccumulator, State};
	
handle_handoff_command ({mosaic_cluster_storage, handoff_request, Vnode, Reference, object, Key, PeerObjectStore}, _Sender, State = #state{object_store = ObjectStore})
		when is_pid (Vnode), is_reference (Reference), is_binary (Key), (bit_size (Key) =:= 160), is_pid (PeerObjectStore) ->
	ok = mosaic_transcript:trace_information ("requested object handoff as source...", [{vnode, Vnode}, {key, Key}]),
	case mosaic_object_store:migrate (ObjectStore, PeerObjectStore, Key) of
		ok ->
			ok = mosaic_transcript:trace_information ("succeeded object handoff as source;", [{vnode, Vnode}, {key, Key}]),
			Vnode ! {mosaic_cluster_storage, handoff_request, Reference, succeeded},
			{reply, ok, State};
		Error = {error, Reason} ->
					ok = mosaic_transcript:trace_error ("failed object handoff as source; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
			Vnode ! {mosaic_cluster_storage, handoff_request, Reference, failed, Reason},
			{reply, Error, State}
	end;
	
handle_handoff_command (Request, Sender, State = #state{}) ->
	ok = mosaic_transcript:trace_warning ("received unknown command request during handoff; forwarding!", [{request, Request}, {sender, Sender}]),
	{forward, State}.


handle_handoff_data (DataBinary, State = #state{object_store = ObjectStore}) ->
	DataTerm = try
		erlang:binary_to_term (DataBinary)
	catch
		error : badarg ->
			{error, {invalid_handoff_data, DataBinary}}
	end,
	case DataTerm of
		{mosaic_cluster_storage, Vnode, object, Key} when is_pid (Vnode), is_binary (Key), (bit_size (Key) =:= 160) ->
			Reference = erlang:make_ref (),
			ok = mosaic_transcript:trace_information ("requesting object handoff as target...", [{vnode, Vnode}, {key, Key}]),
			ok = riak_core_vnode:send_command (Vnode, {mosaic_cluster_storage, handoff_request, erlang:self (), Reference, object, Key, ObjectStore}),
			receive
				{mosaic_cluster_storage, handoff_request, Reference, succeeded} ->
					ok = mosaic_transcript:trace_information ("succeeded object handoff as target;", [{vnode, Vnode}, {key, Key}]),
					{reply, ok, State};
				{mosaic_cluster_storage, handoff_request, Reference, failed, Reason} ->
					ok = mosaic_transcript:trace_error ("failed object handoff as target; ignoring!", [{vnode, Vnode}, {key, Key}, {reason, Reason}]),
					{reply, {error, Reason}, State}
			end;
		Error = {error, Reason} ->
			ok = mosaic_transcript:trace_error ("received invalid handoff data", [{data, DataBinary}, {reason, Reason}]),
			{reply, Error, State};
		_ ->
			ok = mosaic_transcript:trace_error ("received invalid handoff data", [{data, DataBinary}]),
			{reply, {error, {invalid_handoff_data, DataTerm}}, State}
	end.


encode_handoff_item (Key, DataTerm = {mosaic_cluster_storage, Vnode, object, Key})
		when is_pid (Vnode), is_binary (Key), (bit_size (Key) =:= 160) ->
	erlang:term_to_binary (DataTerm).
