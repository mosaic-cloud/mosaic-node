
-module (mosaic_cluster_tests).

-export ([test/0, execute/1]).


test () ->
	ok = try
		ok = case application:load (mosaic_cluster) of
			ok ->
				ok;
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end,
		{ok, Scenario, Actions} = case application:get_env (mosaic_cluster, tests_scenario) of
			{ok, boot} ->
				{ok, defaults, [
						{boot}, {ping, 4}]};
			{ok, create_dummy_1} ->
				{ok, defaults, [
						{boot}, {ping, 4},
						{define_and_create_processes, dummy, term, defaults, 1}]};
			{ok, create_dummy_16} ->
				{ok, defaults, [
						{boot}, {ping, 4},
						{define_and_create_processes, dummy, term, defaults, 16}]};
			{ok, create_abacus_1} ->
				{ok, defaults, [
						{boot}, {ping, 4},
						{define_and_create_processes, abacus, term, defaults, 1}]};
			{ok, ring_join_leave} ->
				Self = erlang:node (),
				case application:get_env (mosaic_cluster, tests_nodes) of
					{ok, [Self | _Peers]} ->
						{ok, ring_join_leave_master, [
								{boot}, {ping, 4}]};
					{ok, Nodes} ->
						Peers = lists:delete (Self, Nodes),
						{ok, ring_join_leave_slaves, [
								{boot}, {ping, 4},
								{sleep, 2 * 1000}, {ring, include, Peers},
								{sleep, 2 * 10}, {ping, 4},
								{sleep, 2 * 1000}, {ring, exclude, Self},
								{sleep, 2 * 10}, {ping, 4},
								{sleep, 2 * 1000},
								{exit}]};
					undefined ->
						throw ({error, undefined_nodes})
				end;
			{ok, Scenario_} ->
				throw ({error, {invalid_scenario, Scenario_}});
			undefined ->
				throw ({error, undefined_scenario})
		end,
		Tests = lists:map (fun (Action) -> {mosaic_cluster_tests, execute, [Action], infinity} end, Actions),
		case mosaic_tests:test_scenario (Scenario, Tests) of
			ok ->
				ok;
			Error2 = {error, _Reason2} ->
				throw (Error2)
		end,
		ok
	catch
		throw : _Error3 = {error, Reason3} ->
			ok = mosaic_tools:trace_error ("failed executing scenario; stopping!", [{reason, Reason3}]),
			ok = timer:sleep (100),
			ok = init:halt (),
			ok
	end,
	ok.


execute ({boot}) ->
	ok = mosaic_cluster:boot (),
	ok = mosaic_process_configurator:register (dummy, term, mosaic_process_tests, configure, void),
	ok = mosaic_process_configurator:register (dummy, json, mosaic_process_tests, configure, void),
	ok = mosaic_process_configurator:register (abacus, term, mosaic_component_process_tests, configure, [{router, mosaic_process_router}]),
	ok = mosaic_process_configurator:register (abacus, json, mosaic_component_process_tests, configure, [{router, mosaic_process_router}]),
	ok;
	
execute ({activate}) ->
	ok = mosaic_executor:service_activate (),
	ok = mosaic_cluster:node_activate (),
	ok;
	
execute ({deactivate}) ->
	ok = mosaic_executor:service_deactivate (),
	ok = mosaic_cluster:node_deactivate (),
	ok;
	
execute ({ring, include, Node})
		when is_atom (Node) ->
	ok = mosaic_cluster:ring_include (Node),
	ok;
	
execute ({ring, include, []}) ->
	ok;
	
execute ({ring, include, [Node | Nodes]})
		when is_atom (Node), is_list (Nodes) ->
	ok = execute ({ring, include, Node}),
	execute ({ring, include, Nodes});
	
execute ({ring, exclude, Node})
		when is_atom (Node) ->
	ok = mosaic_cluster:ring_exclude (Node),
	ok;
	
execute ({ring, exclude, [Node | Nodes]})
		when is_atom (Node), is_list (Nodes) ->
	ok = execute ({ring, exclude, Node}),
	execute ({ring, exclude, Nodes});
	
execute ({ring, exclude, self}) ->
	execute ({ring, exclude, erlang:node ()});
	
execute ({ring, reboot}) ->
	ok = mosaic_cluster:ring_reboot (),
	ok;
	
execute ({ping, Count}) ->
	ok = case mosaic_executor:ping (Count) of
		{ok, _, []} ->
			ok;
		{ok, _, Reasons} ->
			erlang:exit ({error, Reasons})
	end,
	ok;
	
execute ({define_and_create_processes, Type, ArgumentsEncoding, ArgumentsContent, Count}) ->
	ok = case mosaic_executor:define_and_create_processes (Type, ArgumentsEncoding, ArgumentsContent, Count) of
		{ok, _, _, []} ->
			ok;
		{ok, _, _, Reasons} ->
			erlang:exit ({error, Reasons})
	end,
	ok;
	
execute ({sleep, Timeout}) ->
	ok = mosaic_tests:sleep (Timeout),
	ok;
	
execute ({exit}) ->
	ok = init:stop (),
	ok.
