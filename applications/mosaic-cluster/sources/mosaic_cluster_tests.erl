
-module (mosaic_cluster_tests).

-export ([test/0]).


test () ->
	ok = mosaic_cluster:boot (),
	ScenariosProfile = discover,
	{ok, Scenarios} = case erlang:node () of
		'nonode@nohost' ->
			{ok, [{wm}, {up}, {ping, 16}]};
		Node ->
			case application:get_env (mosaic_cluster, nodes) of
				{ok, Nodes} ->
					case ScenariosProfile of
						single ->
							{ok, [
									{wm}, {up}, {ping, 16}
								]};
						discover ->
							{ok, [
									{wm}, {up}, {da}, {ping, 16}
								]};
						join_leave ->
							case Nodes of
								[Node | _] ->
									{ok, [
											{wm}, {up}, {ping, 16},
											{sleep, 2 * 1000}, {join, Nodes},
											{sleep, 2 * 10}, {ping, 16}
										]};
								_ ->
									{ok, [
											{wm}, {up}, {ping, 16},
											{sleep, 2 * 1000}, {join, Nodes},
											{sleep, 2 * 10}, {ping, 16},
											{sleep, 2 * 1000}, {leave},
											{sleep, 2 * 10}, {ping, 16},
											{sleep, 2 * 1000}, {exit}]}
							end
					end;
				undefined ->
					{ok, [{wm}, {up}]}
			end
	end,
	OldTrapExit = erlang:process_flag (trap_exit, true),
	Slave = erlang:spawn_link (
			fun () ->
				ok = lists:foreach (
						fun (Scenario) ->
							ok = mosaic_tools:report_info (mosaic_cluster, test, scenario, Scenario),
							ok = test (Scenario)
						end,
						lists:flatten (Scenarios))
			end),
	ok = receive
		{'EXIT', Slave, normal} ->
			ok;
		{'EXIT', Slave, Reason} ->
			ok = mosaic_tools:report_error (mosaic_cluster, test, error, Reason),
			ok
	end,
	true = erlang:process_flag (trap_exit, OldTrapExit),
	ok.


test ({wm}) ->
	ok = mosaic_webmachine:enforce_started (),
	ok;
	
test ({da}) ->
	{ok, _} = mosaic_discovery_events:start_supervised (),
	JoinFun = fun (Message, void) ->
			case Message of
				{broadcasted, {mosaic_cluster, {join, Node}}} when is_atom (Node) ->
					% ok = mosaic_tools:report_info (mosaic_cluster_tests, test, join, {Node}),
					ok = mosaic_cluster:ring_include (Node),
					{ok, void};
				_ ->
					{ok, void}
			end
	end,
	ok = mosaic_discovery_events:register_handler (mosaic_discovery_events, {JoinFun, void}),
	{ok, _} = mosaic_discovery_agent:start_supervised (),
	ok = mosaic_discovery_agent:broadcast ({mosaic_cluster, {join, erlang:node ()}}),
	ok;
	
test ({up}) ->
	ok = mosaic_executor:service_activate (),
	ok = mosaic_cluster:node_activate (),
	ok;
	
test ({down}) ->
	ok = mosaic_executor:service_deactivate (),
	ok = mosaic_cluster:node_deactivate (),
	ok;
	
test ({join, Nodes}) ->
	ok = lists:foreach (fun (Node) -> _ = mosaic_cluster:ring_include (Node) end, Nodes),
	ok;
	
test ({leave}) ->
	ok = mosaic_cluster:ring_exclude (erlang:node ()),
	ok;
	
test ({ping, Count}) ->
	{ok, _, _} = mosaic_executor:ping (Count),
	ok;
	
test ({define_and_create_dummy_processes, Count}) ->
	{ok, _, _, _} = mosaic_executor:define_and_create_processes (mosaic_dummy_process, defaults, Count),
	ok;
	
test ({sleep, Timeout}) ->
	ok = timer:sleep (Timeout),
	ok;
	
test ({exit}) ->
	ok = init:stop (),
	ok.
