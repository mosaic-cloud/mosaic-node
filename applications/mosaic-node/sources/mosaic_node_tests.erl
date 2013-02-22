
-module (mosaic_node_tests).


-export ([test/0]).


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
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, discovery}, {start, wui}]};
			{ok, Enforcement} when (Enforcement =:= 'enforce-node-by-name') orelse (Enforcement =:= 'enforce-node-by-index') ->
				{ok, Executable} = mosaic_generic_coders:os_bin_get (<<"mosaic_port_process_dummy.elf">>),
				{ok, Nodes} = application:get_env (mosaic_node, tests_nodes),
				Node = erlang:node (),
				NodeBinary = erlang:atom_to_binary (erlang:node (), 'utf8'),
				NodeSelector = case Enforcement of
					'enforce-node-by-name' ->
						NodeBinary;
					'enforce-node-by-index' ->
						lists:foldl (fun (OtherNode, Count) when (OtherNode < Node) -> Count + 1; (_, Count) -> Count end, 0, Nodes)
				end,
				Timeout = <<"30s">>,
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{start, discovery},
						{sleep, 2 * 1000},
						{ring, wait_stable},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-tests:exec', json, [Executable, [Timeout, NodeBinary]], {json, {struct, [{<<"mosaic:enforced-node">>, NodeSelector}]}}, 8},
						{sleep, 2 * 1000}]};
			{ok, 'rabbitmq'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1}]};
			{ok, 'riak-kv'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1}]};
			{ok, 'couchdb'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:couchdb', json, null, 1}]};
			{ok, 'riak-kv-4'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 4}]};
			{ok, 'riak-driver'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:java-driver-riak', json, null, 1},
						{sleep, 2 * 1000},
						{call_process, 'mosaic-components:java-driver-riak', <<"mosaic-component:get.channel.data">>, null}]};
			{ok, 'httpg-environment'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:httpg', json, null, 1},
						{sleep, 2 * 1000}]};
			{ok, 'cloudlet-environment'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:httpg', json, null, 1},
						{define_and_create_processes, 'mosaic-components:java-driver-amqp', json, null, 1},
						{define_and_create_processes, 'mosaic-components:java-driver-riak', json, null, 1},
						{sleep, 2 * 1000}]};
			{ok, 'examples-realtime-feeds'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:httpg', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:fetcher', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:indexer', json, null, 1},
						{sleep, 2 * 1000},
						%{define_and_create_processes, 'mosaic-examples-realtime-feeds:scavanger', json, null, 1},
						%{sleep, 2 * 1000},
						%{define_and_create_processes, 'mosaic-examples-realtime-feeds:leacher', json, null, 1},
						%{sleep, 2 * 1000},
						%{define_and_create_processes, 'mosaic-examples-realtime-feeds:pusher', json, null, 1},
						%{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:frontend-java', json, null, 1},
						{sleep, 2 * 1000}]};
			{ok, 'examples-realtime-feeds-java'} ->
				{ok, defaults, [
						{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui},
						{define_and_create_processes, 'mosaic-components:rabbitmq', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:riak-kv', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:httpg', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:fetcher', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:java-driver-amqp', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-components:java-driver-riak', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:indexer-java', json, null, 1},
						{sleep, 2 * 1000},
						{define_and_create_processes, 'mosaic-examples-realtime-feeds:frontend-java', json, null, 1},
						{sleep, 2 * 1000}]};
			{ok, 'ring-join-leave'} ->
				Self = erlang:node (),
				case application:get_env (mosaic_node, tests_nodes) of
					{ok, [Self | _Peers]} ->
						{ok, ring_join_leave_master, [
								{boot}, {ring, reboot}, {activate}, {initialize}, {start, wui}]};
					{ok, [First | _]} ->
						{ok, ring_join_leave_slaves, [
								{boot}, {ring, reboot}, {activate},
								{ring, join, First}, {sleep, 2 * 1000},
								{ring, wait_stable}, {sleep, 2 * 1000},
								{ring, exclude, self}, {sleep, 2 * 1000},
								{ring, wait_stable}, {sleep, 2 * 1000},
								{exit}]};
					undefined ->
						throw ({error, undefined_nodes})
				end;
			{ok, Scenario_} ->
				throw ({error, {invalid_scenario, Scenario_}});
			undefined ->
				throw ({error, undefined_scenario})
		end,
		Tests = lists:map (fun (Action) -> {mosaic_node_scripts, execute, [Action], infinity} end, Actions),
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
