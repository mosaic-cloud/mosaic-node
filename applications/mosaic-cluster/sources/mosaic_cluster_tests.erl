
-module (mosaic_cluster_tests).

-export ([test/0]).


test () ->
	ok = mosaic_cluster:boot (),
	ScenariosProfile = normal,
	{ok, Scenarios} = case erlang:node () of
		'nonode@nohost' ->
			{ok, [{wm}, {up}]};
		Node ->
			case application:get_env (mosaic_cluster, nodes) of
				{ok, Nodes} ->
					case ScenariosProfile of
						normal ->
							{ok, [{wm}, {up}]};
						fuzzy ->
							case Nodes of
								[Node | _] ->
									{ok, [{wm}, {up}, {define_and_create_dummy_processes, 32}, {sleep, 6 * 1000}, {join, Nodes}]};
								_ ->
									{ok, [{wm}, {up}, {sleep, 3 * 1000}, {join, Nodes}, {sleep, 12 * 1000}, {down}, {sleep, 3 * 1000}, {up}]}
							end
					end;
				undefined ->
					{ok, [{wm}, {up}]}
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
	ok = mosaic_executor:service_activate (),
	ok = mosaic_cluster:node_activate (),
	ok;
	
test ({down}) ->
	ok = mosaic_executor:service_deactivate (),
	ok = mosaic_cluster:node_deactivate (),
	ok;
	
test ({wm}) ->
	ok = mosaic_webmachine:enforce_start (),
	ok;
	
test ({join, Nodes}) ->
	ok = lists:foreach (fun (Node) -> _ = mosaic_cluster:ring_include (Node) end, Nodes),
	ok;
	
test ({leave}) ->
	ok = mosaic_cluster:ring_exclude (erlang:node ()),
	ok;
	
test ({sleep, Timeout}) ->
	ok = timer:sleep (Timeout),
	ok.
