
-module (mosaic_cluster).

-export ([nodes/0, partitions/0, targets/4]).
-export ([node_activate/0, node_deactivate/0]).
-export ([ring_include/1, ring_exclude/1]).
-export ([boot/0]).


nodes () ->
	case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			Nodes = riak_core_ring:all_members (Ring),
			{ok, Nodes};
		Error = {error, _Reason} ->
			Error
	end.


partitions () ->
	case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			Partitions = riak_core_ring:all_owners (Ring),
			{ok, Partitions};
		Error = {error, _Reason} ->
			Error
	end.


targets (Key, Service, Count, Type)
		when is_binary (Key), (bit_size (Key) =:= 160), is_atom (Service), is_integer (Count), (Count > 0),
				((Type =:= primaries) orelse (Type =:= active_without_fallbacks) orelse (Type =:= active_with_fallbacks)) ->
	Nodes = riak_core_node_watcher:nodes (Service),
	Outcome = case riak_core_ring_manager:get_my_ring () of
		{ok, Ring} ->
			MaxCount = min (Count, riak_core_ring:num_partitions (Ring)),
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


ring_include (Node)
		when is_atom (Node) ->
	case net_adm:ping (Node) of
		pong ->
			case riak_core_gossip:send_ring (Node) of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		pang ->
			{error, nodedown}
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


boot () ->
	boot (mosaic_cluster).

boot ([]) ->
	ok;
	
boot ([App | RemainingApps])
		when is_atom (App), is_list (RemainingApps) ->
	case boot (App) of
		ok ->
			boot (RemainingApps);
		Error = {error, _Reason} ->
			Error
	end;
	
boot (App)
		when is_atom (App) ->
	ok = case application:load (App) of
		ok ->
			ok;
		{error, {already_loaded, App}} ->
			ok
	end,
	case application:get_key (App, applications) of
		{ok, DepApps} ->
			case boot (DepApps) of
				ok ->
					case application:start (App) of
						ok ->
							ok;
						{error, {already_started, App}} ->
							ok;
						Error = {error, _Reason} ->
							Error
					end;
				Error = {error, _Reason} ->
					Error
			end;
		undefined ->
			{error, {undefined_dependencies, App}}
	end.
