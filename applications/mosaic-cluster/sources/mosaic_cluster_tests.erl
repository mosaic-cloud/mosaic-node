
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
						{boot}, {activate}, {ping, default}, {initialize}]};
			{ok, create_dummy_1} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, dummy, term, defaults, 1}]};
			{ok, create_dummy_4} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, dummy, term, defaults, 4}]};
			{ok, create_python_abacus_4} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, python_abacus, term, defaults, 4}]};
			{ok, create_java_abacus_4} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, java_abacus, term, defaults, 4}]};
			{ok, create_node_abacus_4} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, node_abacus, term, defaults, 4}]};
			{ok, create_rabbitmq} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, rabbitmq, term, defaults, 1}]};
			{ok, ring_join_leave} ->
				Self = erlang:node (),
				case application:get_env (mosaic_cluster, tests_nodes) of
					{ok, [Self | _Peers]} ->
						{ok, ring_join_leave_master, [
								{boot}, {activate}, {ping, 4}, {initialize},
								{define_and_create_processes, dummy, term, defaults, 4}]};
					{ok, Nodes} ->
						Peers = lists:delete (Self, Nodes),
						{ok, ring_join_leave_slaves, [
								{boot}, {activate}, {ping, 4},
								{sleep, 2 * 1000}, {ring, include, Peers},
								{sleep, 2 * 10}, {ping, 4},
								{sleep, 12 * 1000}, {ring, exclude, Self},
								{sleep, 2 * 10}, {ping, 4},
								{sleep, 12 * 1000},
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
	ok = mosaic_cluster_app:boot (),
	ok;
	
execute ({activate}) ->
	ok = mosaic_cluster_processes:service_activate (),
	ok = mosaic_cluster_storage:service_activate (),
	ok = mosaic_cluster_tools:node_activate (),
	ok;
	
execute ({deactivate}) ->
	ok = mosaic_cluster_processes:service_deactivate (),
	ok = mosaic_cluster_storage:service_deactivate (),
	ok = mosaic_cluster_tools:node_deactivate (),
	ok;
	
execute ({initialize}) ->
	ok = mosaic_process_configurator:register (dummy, term, {mosaic_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (dummy, json, {mosaic_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (python_abacus, term, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (python_abacus, json, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (java_abacus, term, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (java_abacus, json, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (node_abacus, term, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (node_abacus, json, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (rabbitmq, term, {mosaic_component_process_tests, configure, defaults}),
	ok = mosaic_process_configurator:register (rabbitmq, json, {mosaic_component_process_tests, configure, defaults}),
	ok;
	
execute ({ring, include, Node})
		when is_atom (Node) ->
	ok = case mosaic_cluster_tools:ring_include (Node) of
		ok ->
			ok;
		{error, nodedown} ->
			ok
	end,
	ok;
	
execute ({ring, include, []}) ->
	ok;
	
execute ({ring, include, [Node | Nodes]})
		when is_atom (Node), is_list (Nodes) ->
	ok = execute ({ring, include, Node}),
	execute ({ring, include, Nodes});
	
execute ({ring, exclude, Node})
		when is_atom (Node) ->
	ok = mosaic_cluster_tools:ring_exclude (Node),
	ok;
	
execute ({ring, exclude, [Node | Nodes]})
		when is_atom (Node), is_list (Nodes) ->
	ok = execute ({ring, exclude, Node}),
	execute ({ring, exclude, Nodes});
	
execute ({ring, exclude, self}) ->
	execute ({ring, exclude, erlang:node ()});
	
execute ({ring, reboot}) ->
	ok = mosaic_cluster_tools:ring_reboot (),
	ok;
	
execute ({ping, Count}) ->
	ok = case mosaic_cluster_processes:service_ping (Count) of
		{ok, _, []} ->
			ok;
		{ok, _, Reasons1} ->
			erlang:exit ({error, Reasons1})
	end,
	ok = case mosaic_cluster_storage:service_ping (Count) of
		{ok, _, []} ->
			ok;
		{ok, _, Reasons2} ->
			erlang:exit ({error, Reasons2})
	end,
	ok;
	
execute ({define_and_create_processes, Type, ConfigurationEncoding, ConfigurationContent, Count}) ->
	ok = case mosaic_cluster_processes:define_and_create (Type, ConfigurationEncoding, ConfigurationContent, Count) of
		{ok, _Processes, []} ->
			ok;
		{ok, _Processes, Reasons} ->
			erlang:exit ({error, Reasons})
	end,
	ok;
	
execute ({sleep, Timeout}) ->
	ok = mosaic_tests:sleep (Timeout),
	ok;
	
execute ({exit}) ->
	ok = init:stop (),
	ok.
