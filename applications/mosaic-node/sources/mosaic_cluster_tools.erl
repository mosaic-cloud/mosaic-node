
-module (mosaic_cluster_tools).


-export ([key/0, key/1, key/2, keys/1, keys/2, keys_for_node/2]).
-export ([service_nodes/1, service_targets/2, service_targets/4, service_keys/2]).
-export ([service_activate/2, service_deactivate/1, service_ping/3]).
-export ([service_request_reply_sync_command/6, service_requests_generator/4, service_replies_collector/1, service_sync_command/4]).
-export ([service_list_sync_command/2, service_map_sync_command/3]).
-export ([service_process_name/3]).
-export ([node_activate/0, node_deactivate/0]).
-export ([ring_nodes/0, ring_size/0, ring_partitions/0, ring_include/1, ring_exclude/1, ring_reboot/0, ring_stable/0]).


key () ->
	mosaic_generic_coders:generate_data (160 div 8).

key (Token) ->
	{ok, chash:key_of (term_to_binary (Token))}.

key (Token, Index)
		when is_integer (Index), (Index >= 0) ->
	key ({Token, Index}).


keys (Count)
		when is_integer (Count), (Count > 0) ->
	{ok, Token} = mosaic_generic_coders:generate_data (160 div 8),
	keys (Token, Count).

keys (Token, Count)
		when is_integer (Count), (Count > 0) ->
	Keys = lists:map (
			fun (Index) -> {ok, Key} = key (Token, Index), Key end,
			lists:seq (0, Count - 1)),
	{ok, Keys}.


keys_for_node (NodeIndex, Count)
		when is_integer (NodeIndex), (NodeIndex >= 0), is_integer (Count), (Count > 0) ->
	{ok, Ring} = riak_core_ring_manager:get_my_ring (),
	Nodes = lists:sort (riak_core_ring:all_members (Ring)),
	Node = lists:nth (NodeIndex + 1, Nodes),
	keys_for_node (Node, Count);
	
keys_for_node (Node, Count)
		when is_atom (Node), is_integer (Count), (Count > 0) ->
	{ok, Ring} = riak_core_ring_manager:get_my_ring (),
	keys_for_node (Node, Count, Ring, []).

keys_for_node (_Node, 0, _Ring, Keys) ->
	{ok, Keys};
	
keys_for_node (Node, Count, Ring, Keys) ->
	{ok, Key} = key (),
	case riak_core_ring:preflist (Key, Ring) of
		[{_, Node} | _] ->
			keys_for_node (Node, Count - 1, Ring, [Key | Keys]);
		[_ | _] ->
			keys_for_node (Node, Count, Ring, Keys)
	end.


service_nodes (Service)
		when is_atom (Service) ->
	Nodes_1 = riak_core_node_watcher:nodes (Service),
	Nodes_2 = ordsets:from_list (Nodes_1),
	{ok, Nodes_2}.


service_targets (Service, Type) ->
	service_targets (Service, <<1461501637330902918203684832716283019655932542975:160>>, all, Type).

service_targets (Service, Key, Count, Type)
		when is_atom (Service), is_binary (Key), (bit_size (Key) =:= 160),
				((Count =:= all) orelse (is_integer (Count) andalso (Count > 0))),
				((Type =:= primaries) orelse (Type =:= active_without_fallbacks) orelse (Type =:= active_with_fallbacks)) ->
	Nodes = riak_core_node_watcher:nodes (Service),
	Outcome = case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			{ok, MaxCount} = case Count of
				all ->
					{ok, riak_core_ring:num_partitions (Ring)};
				_ ->
					{ok, min (Count, riak_core_ring:num_partitions (Ring))}
			end,
			case Type of
				primaries ->
					{Targets_, _} = lists:split (MaxCount, riak_core_ring:preflist (Key, Ring)),
					{ok, Targets_};
				active_without_fallbacks ->
					Targets_ = riak_core_apl:get_primary_apl (Key, MaxCount, Ring, Nodes),
					{ok, Targets_};
				active_with_fallbacks_ ->
					Targets_ = riak_core_apl:get_apl (Key, MaxCount, Ring, Nodes),
					{ok, Targets_}
			end;
		Error = {error, _Reason1} ->
			Error
	end,
	case Outcome of
		{ok, Targets} when is_list (Targets) ->
			Outcome;
		{error, _Reason2} ->
			Outcome
	end.


service_keys (Service, Type)
		when is_atom (Service),
				((Type =:= primaries) orelse (Type =:= active_without_fallbacks) orelse (Type =:= active_with_fallbacks)) ->
	case service_targets (Service, Type) of
		{ok, Targets} ->
			Keys = lists:map (fun ({Partition, _Node}) -> <<(Partition - 1) : 160>> end, Targets),
			{ok, Keys};
		Error = {error, _Reason} ->
			Error
	end.


service_activate (Service, VnodeModule)
		when is_atom (Service), is_atom (VnodeModule) ->
	case mosaic_node_sup:start_child_vnode_master (VnodeModule) of
		{ok, Master} when is_pid (Master) ->
			ok = riak_core:register_vnode_module (VnodeModule),
			ok = riak_core_node_watcher:service_up (Service, Master),
			ok;
		{error, already_started} ->
			MasterName = riak_core_vnode_master:reg_name (VnodeModule),
			case erlang:whereis (MasterName) of
				Master when is_pid (Master) ->
					ok = riak_core_node_watcher:service_up (Service, Master),
					ok;
				undefined ->
					{error, vnode_master_not_started}
			end;
		Error = {error, _Reason} ->
			Error
	end.


service_deactivate (Service)
		when is_atom (Service) ->
	case riak_core_node_watcher:service_down (Service) of
		ok ->
			ok;
		Error = {error, _Reason} ->
			Error
	end.


service_ping (Service, VnodeModule, default) ->
	service_ping (Service, VnodeModule, 4);
	
service_ping (Service, VnodeModule, Count)
		when is_atom (Service), is_atom (VnodeModule), is_integer (Count), (Count > 0) ->
	{ok, Keys} = keys (Count),
	{ok, [], {Pongs, Pangs}} = service_sync_command (
			fun
				([]) ->
					{finish, []};
				([Key | PendingKeys]) ->
					{ok, Targets} = service_targets (Service, Key, 1, primaries),
					Command = {mosaic_cluster, ping, Key},
					Request = {Key, Command, Service, VnodeModule, Targets},
					{continue, Request, PendingKeys}
			end, Keys,
			fun ({Key, {mosaic_cluster, ping, Key}, _, _, _}, Replies, {CollectedPongs, CollectedPangs}) ->
				Outcomes = lists:map (
						fun ({Target, Reply}) ->
							case Reply of
								Pong = {pong, Key, Vnode, Service, Target} when is_pid (Vnode) ->
									Pong;
								{pong, OtherKey, Vnode, _Service, _Target} when is_pid (Vnode) ->
									{pang, Key, Target, {mismatched_key, OtherKey}};
								{pong, Key, Vnode, OtherService, Target} when is_pid (Vnode) ->
									{pang, Key, Target, {mismatched_service, OtherService}};
								{pong, Key, Vnode, Service, OtherTarget} when is_pid (Vnode) ->
									{pang, Key, Target, {mismatched_target, OtherTarget}};
								{error, Reason} ->
									{pang, Key, Target, Reason};
								_ ->
									{pang, Key, Target, {invalid_reply, Reply}}
							end
						end,
						Replies),
				Pongs = lists:filter (fun ({pong, _, _, _, _}) -> true; ({pang, _, _, _}) -> false end, Outcomes),
				Pangs = lists:filter (fun ({pang, _, _, _}) -> true; ({pong, _, _, _, _}) -> false end, Outcomes),
				{continue, {Pongs ++ CollectedPongs, Pangs ++ CollectedPangs}}
			end, {[], []}),
	{ok, lists:reverse (Pongs), lists:reverse (Pangs)}.


service_request_reply_sync_command (CommandFunction, OutcomeFunction, Keys, Service, VnodeModule, Fanout) ->
	{ok, [], {Outcomes, Reasons}} = service_sync_command (
			service_requests_generator (CommandFunction, Service, VnodeModule, Fanout), Keys,
			service_replies_collector (OutcomeFunction), {[], []}),
	{ok, lists:reverse (Outcomes), lists:reverse (Reasons)}.


service_requests_generator (CommandFunction, Service, VnodeModule, Fanout)
		when is_function (CommandFunction, 1), is_atom (Service), is_atom (VnodeModule), is_integer (Fanout), (Fanout > 0) ->
	fun
		([]) ->
			{finish, []};
		([Key | PendingKeys]) ->
			{ok, Targets} = service_targets (Service, Key, Fanout, active_without_fallbacks),
			Command = CommandFunction (Key),
			Request = {Key, Command, Service, VnodeModule, Targets},
			{continue, Request, PendingKeys}
	end.


service_replies_collector (OutcomeFunction)
		when is_function (OutcomeFunction, 3) ->
	fun ({Key, Command, _Service, _VnodeModule, _Targets}, TargetReplies, {CollectedOutcomes, CollectedReasons}) ->
		Values = lists:map (
				fun ({Target, Reply}) ->
					case OutcomeFunction (Key, Command, Reply) of
						Value = {outcome, _Outcome} ->
							Value;
						{error, Reason} ->
							{error, Target, Key, Reason}
					end
				end, TargetReplies),
		Outcomes = lists:map (
				fun ({outcome, Outcome}) -> Outcome end,
				lists:filter (fun ({outcome, _}) -> true; ({error, _, _, _}) -> false end, Values)),
		Reasons = lists:map (
				fun ({error, Target, Key_, Reason}) when Key_ =:= Key -> {Target, Key_, Reason} end,
				lists:filter (fun ({error, _, _, _}) -> true; ({outcome, _}) -> false end, Values)),
		{continue, {Outcomes ++ CollectedOutcomes, Reasons ++ CollectedReasons}}
	end.


service_sync_command (RequestFunction, RequestInputState, RepliesFunction, RepliesInputState)
		when is_function (RequestFunction, 1), is_function (RepliesFunction, 3) ->
	case RequestFunction (RequestInputState) of
		{continue, Request = {Key, Command, Service, VnodeModule, Targets}, RequestOutputState}
				when is_binary (Key), (bit_size (Key) =:= 160), is_atom (Service), is_atom (VnodeModule), is_list (Targets) ->
			TargetReplies = lists:map (
					fun (Target = {Partition, Node})
							when is_atom (Node), is_integer (Partition),
									(Partition >= 0), (Partition < 1461501637330902918203684832716283019655932542975) ->
						Reply = try
							riak_core_vnode_master:sync_command (Target, Command, riak_core_vnode_master:reg_name (VnodeModule))
						catch
							exit : {Reason, _Call} ->
								{error, Reason}
						end,
						{Target, Reply}
					end, Targets),
			case RepliesFunction (Request, TargetReplies, RepliesInputState) of
				{continue, RepliesOutputState} ->
					service_sync_command (RequestFunction, RequestOutputState, RepliesFunction, RepliesOutputState);
				{finish, RepliesOutputState} ->
					{ok, RequestOutputState, RepliesOutputState}
			end;
		{finish, RequestOutputState} ->
			{ok, RequestOutputState, RepliesInputState}
	end.


service_list_sync_command (Service, VnodeModule)
		when is_atom (Service), is_atom (VnodeModule) ->
	{ok, TargetKeys} = service_keys (Service, primaries),
	{ok, Keys, Reasons} = service_request_reply_sync_command (
			fun (_TargetKey) -> {mosaic_cluster, list} end,
			fun (_TargetKey, _Request, Reply) ->
				case Reply of
					{ok, Keys} when is_list (Keys) ->
						{outcome, Keys};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			TargetKeys, Service, VnodeModule, 1),
	{ok, lists:usort (lists:append (Keys)), Reasons}.


service_map_sync_command (Service, VnodeModule, Mapper)
		when is_atom (Service), is_atom (VnodeModule), is_function (Mapper, 2) ->
	{ok, TargetKeys} = service_keys (Service, primaries),
	{ok, Objects, Reasons} = service_request_reply_sync_command (
			fun (_TargetKey) -> {mosaic_cluster, map, Mapper} end,
			fun (_TargetKey, _Request, Reply) ->
				case Reply of
					{ok, Objects} when is_list (Objects) ->
						{outcome, Objects};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			TargetKeys, Service, VnodeModule, 1),
	{ok, lists:append (Objects), Reasons}.


service_process_name (Service, Type, Partition)
		when is_atom (Service), is_atom (Type),
				is_integer (Partition), (Partition >= 0), (Partition < 1461501637330902918203684832716283019655932542976) ->
	{ok, PartitionCount} = ring_size (),
	PartitionBitsInexact = math:log (PartitionCount) / math:log (2),
	PartitionBitsTruncated = erlang:trunc (PartitionBitsInexact),
	if
		(PartitionBitsInexact == PartitionBitsTruncated) ->
			PartitionBits = PartitionBitsTruncated;
		true ->
			PartitionBits = PartitionBitsTruncated + 1
	end,
	if
		((PartitionBits rem 8) =:= 0) ->
			PartitionBitsPadded = PartitionBits;
		true ->
			PartitionBitsPadded = ((PartitionBits div 8) + 1) * 8
	end,
	PartitionHex = string:to_lower (erlang:integer_to_list (Partition bsr (160 - PartitionBitsPadded), 16)),
	PartitionHexPadded = (lists:duplicate ((PartitionBitsPadded div 4) - erlang:length (PartitionHex), $0) ++ PartitionHex),
	{ok, erlang:list_to_atom ((erlang:atom_to_list (Service) ++ "#" ++ erlang:atom_to_list (Type) ++ "#" ++ PartitionHexPadded))}.


node_activate () ->
	case riak_core_node_watcher:node_up () of
		ok ->
			ok;
		Error = {error, _Reason} ->
			Error
	end.


node_deactivate () ->
	case riak_core_node_watcher:node_down () of
		ok ->
			ok;
		Error = {error, _Reason} ->
			Error
	end.


ring_nodes () ->
	case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			Nodes_1 = riak_core_ring:all_members (Ring),
			Nodes_2 = ordsets:from_list (Nodes_1),
			{ok, Nodes_2};
		Error = {error, _Reason} ->
			Error
	end.


ring_size () ->
	case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			Size = riak_core_ring:num_partitions (Ring),
			{ok, Size};
		Error = {error, _Reason} ->
			Error
	end.


ring_partitions () ->
	case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			Partitions_1 = riak_core_ring:all_owners (Ring),
			Partitions_2 = ordsets:from_list (Partitions_1),
			{ok, Partitions_2};
		Error = {error, _Reason} ->
			Error
	end.


ring_include (Node)
		when is_atom (Node) ->
	{ok, Nodes} = ring_nodes (),
	AlreadyMember = ordsets:is_element (Node, Nodes),
	if
		not AlreadyMember ->
			case net_adm:ping (Node) of
				pong ->
					{ok, Ring} = riak_core_ring_manager:get_my_ring (),
					gen_server:cast({riak_core_gossip, Node}, {reconcile_ring, Ring}),
					gen_server:cast({riak_core_gossip, Node}, {send_ring_to, erlang:node ()}),
					ok;
				pang ->
					{error, nodedown}
			end;
		true ->
			{error, already_member}
	end.


ring_exclude (Node)
		when is_atom (Node) ->
	try riak_core_gossip:remove_from_cluster (Node) of
		_ ->
			ok
	catch
		error : badarg ->
			ok
	end.


ring_stable () ->
	% FIXME: This is an incredible ugly hack...
	try
		ring_stable_1 ()
	catch _ : Reason ->
		ok = mosaic_transcript:trace_warning ("ring stability check failed; retrying after a timeout!", [{reason, Reason}]),
		ok = timer:sleep (1000),
		ring_stable ()
	end.

ring_stable_1 () ->
	{ok, RingNodes} = ring_nodes (),
	Nodes = ordsets:union (RingNodes, ordsets:from_list ([erlang:node () | erlang:nodes ()])),
	{PartitionReplies, FailedNodes} = rpc:multicall (Nodes, mosaic_cluster_tools, ring_partitions, []),
	AllPartitions = [Partition || {ok, Partition} <- PartitionReplies],
	case FailedNodes of
		[] ->
			UniquePartitions = lists:foldl (
					fun (Partition, OldCache) ->
						case ordsets:is_element (Partition, OldCache) of
							true ->
								OldCache;
							false ->
								ordsets:add_element (Partition, OldCache)
						end
					end,
					ordsets:new (),
					AllPartitions),
			case UniquePartitions of
				[Partitions] ->
					OwnedPartitions = orddict:from_list (
							[
								{Node, ordsets:from_list ([Partition || {Partition, Node_1} <- Partitions, Node_1 =:= Node])}
								|| Node <- Nodes]),
					ActivePartitions = orddict:from_list (
							[
								{Node, ordsets:from_list (
										[
											Partition || {_, Partition} <-
												[
													rpc:call (Node, riak_core_vnode, get_mod_index, [Vnode])
													|| {_, Vnode, _, _} <- supervisor:which_children ({riak_core_vnode_sup, Node})]])}
								|| Node <- Nodes]),
					TransferringPartitions = lists:foldl (
							fun (Node, TransferringPartitions) ->
								NodeOwnedPartitions = orddict:fetch (Node, OwnedPartitions),
								NodeActivePartitions = orddict:fetch (Node, ActivePartitions),
								case
										ordsets:union (
												ordsets:subtract (NodeOwnedPartitions, NodeActivePartitions),
												ordsets:subtract (NodeActivePartitions, NodeOwnedPartitions)) of
									[] ->
										TransferringPartitions;
									NodeTransferringPartitions ->
										orddict:store (Node, NodeTransferringPartitions, TransferringPartitions)
								end
							end,
							orddict:new (),
							Nodes),
					case TransferringPartitions of
						[] ->
							ok;
						_ ->
							{error, {transferring, TransferringPartitions}}
					end;
				_ ->
					{error, {diverging, UniquePartitions}}
			end;
		_ ->
			{error, {nodedown, FailedNodes}}
	end.


ring_reboot () ->
	NewRing = riak_core_ring:fresh (),
	ok = application:set_env (riak_core, wants_claim_fun, {riak_core_claim, default_wants_claim}),
	ok = riak_core_ring_manager:set_my_ring (NewRing),
	ok.
