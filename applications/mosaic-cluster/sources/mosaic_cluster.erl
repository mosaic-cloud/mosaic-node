
-module (mosaic_cluster).

-export ([ring_include/1, ring_exclude/1]).
-export ([test/0, boot/0, join/0, leave/0]).


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


test () ->
	ok = boot (),
	{ok, Scenarios} = case erlang:node () of
		'nonode@nohost' ->
			{ok, [{define_and_create_dummy_processes, 16}]};
		Node ->
			case application:get_env (mosaic_cluster, nodes) of
				{ok, Nodes} ->
					case Nodes of
						[Node | _] ->
							%{ok, [{up}, {wm}, {define_and_create_dummy_processes, 32}, {sleep, 6 * 1000}, {join, Nodes}]};
							{ok, [{up}, {wm}]};
						_ ->
							%{ok, [{up}, {sleep, 3 * 1000}, {join, Nodes}, {sleep, 12 * 1000}, {leave}]}
							{ok, [{up}]}
					end;
				undefined ->
					{ok, []}
			end
	end,
	ok = lists:foreach (
			fun (Scenario) ->
				ok = mosaic_tools:report_info (mosaic_cluster, test, scenario, Scenario),
				ok = test (Scenario)
			end, Scenarios),
	ok.

test ({define_and_create_dummy_processes, Count}) ->
	{ok, _, _} = mosaic_executor:define_and_create_processes (mosaic_dummy_process, defaults, Count),
	ok;
	
test ({up}) ->
	ok = mosaic_executor_vnode:service_up (),
	ok = riak_core_node_watcher:node_up (),
	ok;
	
test ({wm}) ->
	ok = mosaic_webmachine:enforce_start (),
	ok;
	
test ({join, Nodes}) ->
	ok = join (Nodes),
	ok;
	
test ({leave}) ->
	ok = leave (),
	ok;
	
test ({sleep, Timeout}) ->
	ok = timer:sleep (Timeout),
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


join () ->
	{ok, Nodes} = application:get_env (mosaic_cluster, nodes),
	join (Nodes).

join (Nodes) ->
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
