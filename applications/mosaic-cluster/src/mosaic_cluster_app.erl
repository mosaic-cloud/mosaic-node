
-module (mosaic_cluster_app).

-behaviour (application).

-export ([boot/0, join/0, leave/0]).
-export ([start/2, stop/1]).


boot () ->
	case boot (mosaic_cluster) of
		ok ->
			case erlang:node () of
				'nonode@nohost' ->
					ok = mosaic_executor_vnode:service_up (),
					ok = riak_core_node_watcher:node_up (),
					ok;
				Node ->
					ok = case application:get_env (mosaic_cluster, nodes) of
						{ok, Nodes} ->
							case lists:member (Node, Nodes) of
								true ->
									ok = mosaic_executor_vnode:service_up (),
									ok = riak_core_node_watcher:node_up (),
									ok = join (),
									{ok, _, _} = mosaic_executor:define_and_create_processes (mosaic_dummy_process, defaults, 2),
									ok;
								false ->
									ok
							end;
						undefined ->
							ok
					end
			end;
		Error = {error, _Reason} ->
			Error
	end.


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


join () ->
	{ok, Nodes} = application:get_env (mosaic_cluster, nodes),
	ok = lists:foreach (
			fun (Node) ->
				case net_adm:ping (Node) of
					pong ->
						ok = riak_core_gossip:send_ring (Node);
					pang ->
						ok
				end
			end, Nodes),
	ok.


leave () ->
	_ = riak_core_gossip:remove_from_cluster (erlang:node ()),
	ok.


start (normal, defaults) ->
	case mosaic_cluster_sup:start_link () of
		{ok, Supervisor} ->
			{ok, Supervisor, void};
		Error = {error, _Reason} ->
			Error
	end;
	
start (normal, Arguments) ->
	{error, {invalid_start_arguments, Arguments}};
	
start (Type, _Arguments) ->
	{error, {invalid_start_type, Type}}.


stop (void) ->
	ok.
