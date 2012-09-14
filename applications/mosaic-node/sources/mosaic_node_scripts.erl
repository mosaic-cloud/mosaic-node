
-module (mosaic_node_scripts).


-export ([start/0, run/1, execute/1]).


start () ->
	try
		ok = case application:load (mosaic_node) of
			ok ->
				ok;
			Error1 = {error, _Reason1} ->
				throw (Error1)
		end,
		ok = case run (default) of
			ok ->
				ok;
			Error2 = {error, _Reason2} ->
				throw (Error2)
		end,
		ok
	catch throw : {error, Reason} ->
		ok = mosaic_transcript:trace_error ("failed starting; stopping!", [{reason, Reason}]),
		ok = mosaic_application_tools:shutdown_async (0),
		ok
	end.


run ([]) ->
	ok;
	
run ([Action | Actions]) ->
	try execute (Action) of
		ok ->
			run (Actions);
		Error = {error, _Reason} ->
			Error
	catch
		throw : Reason -> {error, {caught, throw, Reason, erlang:get_stacktrace ()}};
		error : Reason -> {error, {caught, error, Reason, erlang:get_stacktrace ()}};
		exit : Reason -> {error, {caught, exit, Reason, erlang:get_stacktrace ()}}
	end;
	
run (default) ->
	case application:get_env (mosaic_node, script) of
		{ok, Script} ->
			run (Script);
		undefined ->
			{error, {unknown_script, default}}
	end;
	
run (boot) ->
	run ([
			{boot},
			{activate},
			{start, discovery},
			% {sleep, 1 * 1000},
			{initialize},
			{start, wui}]);
	
run (none) ->
	run ([]);
	
run (Script)
		when is_atom (Script) ->
	{error, {unknown_script, Script}};
	
run (Script) ->
	{error, {invalid_script, Script}}.


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
	
execute ({start, System}) ->
	case System of
		supervisor ->
			mosaic_node_app:start_supervisor ();
		daemons ->
			mosaic_node_app:start_daemons ();
		discovery ->
			mosaic_node_app:start_discovery ();
		wui ->
			mosaic_node_app:start_wui ()
	end;
	
execute ({initialize}) ->
	execute ({define, defaults});
	
execute ({define, {process_alias, Alias, Identifier}}) ->
	ok = mosaic_process_router:register_alias (Alias, Identifier);
	
execute ({define, {process_configurator, Type, ConfigurationEncoding, Function}}) ->
	ok = mosaic_process_configurator:register (Type, ConfigurationEncoding, Function);
	
execute ({define, {process_configurator, Type, ConfigurationEncoding, Function, Annotation}}) ->
	ok = mosaic_process_configurator:register (Type, ConfigurationEncoding, Function, Annotation);
	
execute ({define, defaults}) ->
	{ok, _, DefinitionsData} = mosaic_static_resources:contents (<<"/definitions.term">>),
	execute ({define, DefinitionsData});
	
execute ({define, DefinitionsData})
		when is_binary (DefinitionsData) ->
	{ok, Definitions} = mosaic_generic_coders:parse_terms (DefinitionsData),
	ok = lists:foreach (
			fun (Definition) ->
				ok = execute ({define, Definition})
			end,
			Definitions),
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
	ok = case mosaic_cluster_processes:define_and_create (Type, ConfigurationEncoding, ConfigurationContent, undefined, Count) of
		{ok, _Processes, []} ->
			ok;
		{ok, _Processes, Reasons} ->
			erlang:exit ({error, Reasons})
	end,
	ok;
	
execute ({define_and_create_processes, Type, ConfigurationEncoding, ConfigurationContent, Annotation, Count}) ->
	ok = case mosaic_cluster_processes:define_and_create (Type, ConfigurationEncoding, ConfigurationContent, Annotation, Count) of
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
