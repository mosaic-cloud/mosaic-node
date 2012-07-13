
-module (mosaic_node_tests).


-export ([test/0, execute/1]).


test () ->
	ok = try
		ok = case application:load (mosaic_node) of
			ok ->
				ok;
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end,
		{ok, Scenario, Actions} = case application:get_env (mosaic_node, tests_scenario) of
			{ok, boot} ->
				{ok, defaults, [
						{boot}, {activate}, {initialize}]};
			{ok, 'test-rabbitmq'} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1}]};
			{ok, 'test-riak-kv'} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1}]};
			{ok, 'test-couchdb'} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, 'mosaic-components:couchdb', json, null, 1}]};
			{ok, 'test-riak-kv-4'} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 4}]};
			{ok, 'examples-realtime-feeds'} ->
				{ok, defaults, [
						{boot}, {activate}, {ping, default}, {initialize},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:httpg', json, null, 1},
						{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:fetcher', json, null, 1},
						{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:scavanger', json, null, 1},
						{sleep, 1 * 1000},
						%{define_and_create_processes, 'mosaic-examples-realtime-feeds:indexer', json, null, 1},
						%{sleep, 1 * 1000},
						%{define_and_create_processes, 'mosaic-examples-realtime-feeds:leacher', json, null, 1},
						%{sleep, 1 * 1000},
						%{define_and_create_processes, 'mosaic-examples-realtime-feeds:pusher', json, null, 1},
						%{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-components:java-driver-container', json, [<<"amqp">>, null, []], 1},
						{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-components:java-driver-container', json, [<<"kv">>, null, []], 1},
						{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:indexer-java', json, [null], 1},
						{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:frontend-java', json, [null], 1},
						{sleep, 1 * 1000}]};
			{ok, 'test-ring-join-leave'} ->
				Self = erlang:node (),
				case application:get_env (mosaic_node, tests_nodes) of
					{ok, [Self | _Peers]} ->
						{ok, ring_join_leave_master, [
								{boot},
								{activate},
								{ping, 4},
								{initialize}]};
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
		Tests = lists:map (fun (Action) -> {mosaic_node_tests, execute, [Action], infinity} end, Actions),
		case mosaic_tests:test_scenario (Scenario, Tests) of
			ok ->
				ok;
			Error2 = {error, _Reason2} ->
				throw (Error2)
		end,
		ok
	catch
		throw : _Error3 = {error, Reason3} ->
			ok = mosaic_transcript:trace_error ("failed executing scenario; stopping!", [{reason, Reason3}]),
			ok = mosaic_application_tools:shutdown_async (0),
			ok
	end,
	ok.


execute ({boot}) ->
	ok = mosaic_node_app:boot (),
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
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-components:rabbitmq">>, <<16#8cd74b5e4ecd322fd7bbfc762ed6cf7d601eede8 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-components:rabbitmq', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:rabbitmq', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-components:riak-kv">>, <<16#9cdce23e78027ef6a52636da7db820c47e695d11 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-components:riak-kv', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:riak-kv', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-components:couchdb">>, <<16#f867c1725a2845f4a68a1165f430fdaa10b8aa16 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-components:couchdb', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:couchdb', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-components:httpg">>, <<16#0891f3a4b73a16cc5ac6947c56924d3e1dd2395e : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-components:httpg', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:httpg', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-components:java-component-container', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:java-component-container', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-components:java-cloudlet-container', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:java-cloudlet-container', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-components:java-driver-amqp', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:java-driver-amqp', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-components:java-driver-riak', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-components:java-driver-riak', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:fetcher">>, <<16#4cb6ba9f09150c29b590b82b02a5a295ffc742d2 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:fetcher', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:fetcher', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:indexer">>, <<16#d4a751daa35f3661797b3fff37eb12dd4c9a8ce8 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:indexer', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:indexer', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:scavanger">>, <<16#1f0818a9870f401b3795097be1806a8b9c2c2240 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:scavanger', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:scavanger', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:leacher">>, <<16#cdb21a6acac6f9a798d5dab03b9309f92bd15c9d : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:leacher', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:leacher', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:pusher">>, <<16#b3310a2ea81b7ccfc03e38d1fc32fcc634b92735 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:pusher', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:pusher', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:frontend-java">>, <<16#a2e40f0b2c041bc694ace68ace08420d40f9cbc0 : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:frontend-java', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:frontend-java', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_cluster_processes_router:register_alias (<<"mosaic-examples-realtime-feeds:indexer-java">>, <<16#13c025be552e4403add3e0acc626d64d490d9ebe : 160>>),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:indexer-java', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-examples-realtime-feeds:indexer-java', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-tests:java-component', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-tests:java-component', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-tests:socat', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-tests:socat', json, {mosaic_component_process_tests, configure, defaults}),
	
	_ = mosaic_process_configurator:register ('mosaic-tests:exec', term, {mosaic_component_process_tests, configure, defaults}),
	_ = mosaic_process_configurator:register ('mosaic-tests:exec', json, {mosaic_component_process_tests, configure, defaults}),
	
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
	ok = mosaic_application_tools:shutdown_async (0),
	ok.
