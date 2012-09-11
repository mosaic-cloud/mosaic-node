
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
						{define_and_create_processes, 'mosaic-components:java-driver-amqp', json, [null], 1},
						{sleep, 1 * 1000},
						{define_and_create_processes, 'mosaic-components:java-driver-riak', json, [null], 1},
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


execute (Action) ->
	mosaic_node_scripts:execute (Action).
