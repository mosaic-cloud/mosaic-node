
-module (mosaic_cluster).

-export ([ring_include/1, ring_exclude/1]).
-export ([boot/0]).


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
