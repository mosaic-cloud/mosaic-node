
-module (mosaic_cluster_tests).

-export ([test/0]).


test () ->
	ok = try begin
		ok = case application:load (mosaic_cluster) of
			ok ->
				ok;
			Error = {error, _Reason} ->
				throw (Error)
		end,
		{ok, Actions} = case application:get_env (mosaic_cluster, tests_scenario) of
			{ok, normal} ->
				{ok, [
						{boot}, {ping, 4},
						{define_and_create_processes, dummy, term, defaults, 2}]};
			{ok, ring_join_leave} ->
				Self = erlang:node (),
				case application:get_env (mosaic_cluster, tests_nodes) of
					{ok, [Self | _Peers]} ->
						{ok, [
								{boot}, {ping, 4}]};
					{ok, Nodes} ->
						Peers = lists:delete (Self, Nodes),
						{ok, [
								{boot}, {ping, 4},
								{sleep, 2 * 1000}, {ring, include, Peers},
								{sleep, 2 * 10}, {ping, 4},
								{sleep, 2 * 1000}, {ring, exclude, Self},
								{sleep, 2 * 10}, {ping, 4},
								{sleep, 2 * 1000}]};
					undefined ->
						throw ({error, undefined_nodes})
				end;
			{ok, Scenario} ->
				throw ({error, {invalid_scenario, Scenario}});
			undefined ->
				throw ({error, undefined_scenario})
		end,
		ok = test (Actions),
		ok
	end catch
		throw : {error, Reason} ->
			ok = mosaic_tools:report_error (mosaic_cluster, test, error, Reason),
			ok
	end,
	ok.


test (Actions)
		when is_list (Actions) ->
	OldTrapExit = erlang:process_flag (trap_exit, true),
	Slave = erlang:spawn_link (
			fun () ->
				ok = lists:foreach (
						fun (Action) ->
							ok = mosaic_tools:report_info (mosaic_cluster, test, action, Action),
							ok = execute (Action),
							ok
						end,
						lists:flatten (Actions))
			end),
	Outcome = receive
		{'EXIT', Slave, normal} ->
			ok;
		{'EXIT', Slave, Error1 = {error, _Reason1}} ->
			Error1;
		{'EXIT', Slave, Reason1} ->
			{error, Reason1}
	end,
	true = erlang:process_flag (trap_exit, OldTrapExit),
	case Outcome of
		ok ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end.


execute ({boot}) ->
	ok = mosaic_cluster:boot (),
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
	ok = timer:sleep (Timeout),
	ok;
	
execute ({exit}) ->
	ok = init:stop (),
	ok.
