
-module (mosaic_cluster).

-export ([key/0, key/1, key/2, keys/1, keys/2]).
-export ([targets/2, targets/4]).
-export ([nodes/0, partitions/0]).
-export ([node_activate/0, node_deactivate/0]).
-export ([ring_include/1, ring_exclude/1, ring_reboot/0]).
-export ([boot/0]).


key () ->
	key (erlang:make_ref ()).

key (Token) ->
	Key = chash:key_of (term_to_binary (Token)),
	{ok, Key}.

key (Token, Index)
		when is_integer (Index), (Index >= 0) ->
	key ({Token, Index}).

keys (Count)
		when is_integer (Count), (Count > 0) ->
	keys (erlang:make_ref (), Count).

keys (Token, Count)
		when is_integer (Count), (Count > 0) ->
	Keys = lists:map (
			fun (Index) -> {ok, Key} = key (Token, Index), Key end,
			lists:seq (0, Count - 1)),
	{ok, Keys}.


targets (Service, Type) ->
	targets (<<1461501637330902918203684832716283019655932542975:160>>, Service, all, Type).

targets (Key, Service, Count, Type)
		when is_binary (Key), (bit_size (Key) =:= 160), is_atom (Service),
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

ring_reboot () ->
	NewRing = riak_core_ring:fresh (),
	ok = application:set_env (riak_core, wants_claim_fun, {riak_core_claim, default_wants_claim}),
	ok = riak_core_ring_manager:set_my_ring (NewRing),
	ok.


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
