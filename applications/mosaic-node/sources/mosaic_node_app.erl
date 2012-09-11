
-module (mosaic_node_app).

-behaviour (application).


-export ([start/2, stop/1, boot/0]).
-export ([start_supervisor/0, start_daemons/0, start_discovery/0, start_wui/0]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


start (normal, defaults) ->
	try
		{ok, Supervisor} = start_supervisor (),
		ok = start_daemons (),
		% ok = start_discovery (),
		% ok = start_wui (),
		{ok, Supervisor, void}
	catch
		throw : Error = {error, _Reason} ->
			Error
	end;
	
start (normal, Configuration) ->
	{error, {invalid_configuration, Configuration}};
	
start (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


stop (void) ->
	ok.


start_supervisor () ->
	case mosaic_node_sup:start_link () of
		Outcome = {ok, _Supervisor} ->
			Outcome;
		Error = {error, _Reason} ->
			throw (Error)
	end.


start_daemons () ->
	ok = case mosaic_cluster_component_resources:start_supervised () of
		{ok, _Resources} ->
			ok;
		Error1 = {error, _Reason1} ->
			throw (Error1)
	end,
	ok = case mosaic_cluster_processes_configurator:start_supervised () of
		{ok, _Configurator} ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end,
	ok = case mosaic_cluster_processes_router:start_supervised () of
		{ok, _Router} ->
			ok;
		Error3 = {error, _Reason3} ->
			throw (Error3)
	end,
	case mosaic_webmachine:start_supervised () of
		{ok, _Webmachine} ->
			ok;
		Error4 = {error, _Reason4} ->
			throw (Error4)
	end,
	ok.


start_discovery () ->
	JoinFunction = fun (Event, void) ->
			ok = case Event of
				{mosaic_discovery_events, broadcasted, {mosaic_node, node, Node}} when is_atom (Node) ->
					% ok = mosaic_transcript:trace_information ("joining node...", [{node, Node}]),
					ok = case mosaic_cluster_tools:ring_include (Node) of
						ok ->
							ok;
						{error, Reason} ->
							ok = mosaic_transcript:trace_error ("failed joining node; ignoring!", [{node, Node}, {reason, Reason}]),
							ok
					end,
					ok;
				{mosaic_discovery_events, broadcasted, Message} ->
					ok = mosaic_transcript:trace_error ("received invalid broadcast message; ignoring!", [{message, Message}]),
					ok;
				_ ->
					ok = mosaic_transcript:trace_error ("received invalid event; ignoring!", [{event, Event}]),
					ok
			end,
			{ok, void}
	end,
	ok = case mosaic_discovery_events:start_supervised () of
		{ok, _Events} ->
			ok;
		Error1 = {error, _Reason1} ->
			throw (Error1)
	end,
	ok = case mosaic_discovery_agent_udp:start_supervised () of
		{ok, _Agent1} ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end,
	ok = case mosaic_discovery_agent_tcp:start_supervised () of
		{ok, _Agent2} ->
			ok;
		Error3 = {error, _Reason3} ->
			throw (Error3)
	end,
	ok = case mosaic_discovery_events:register_handler (mosaic_discovery_events, {JoinFunction, void}) of
		ok ->
			ok;
		Error4 = {error, _Reason4} ->
			throw (Error4)
	end,
	ok = case mosaic_discovery_agent_udp:broadcast ({mosaic_node, node, erlang:node ()}) of
		{ok, _Reference1} ->
			ok;
		Error5 = {error, _Reason5} ->
			throw (Error5)
	end,
	ok = case mosaic_discovery_agent_tcp:broadcast ({mosaic_node, node, erlang:node ()}) of
		{ok, _Reference2} ->
			ok;
		Error6 = {error, _Reason6} ->
			throw (Error6)
	end,
	ok.


start_wui () ->
	try
		WuiExecutable = try
			enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-node-wui--run-node-wui">>))
		catch throw : _Error1 = {error, _Reason1} -> throw (ok) end,
		{WebmachineSocketIp, WebmachineSocketPort} = enforce_ok_1 (mosaic_generic_coders:application_env_get (webmachine_address, mosaic_node,
				{decode,
						fun
							({Ip, Port}) when is_list (Ip), is_integer (Port) -> {ok, {erlang:list_to_binary (Ip), Port}};
							(Descriptor) -> {error, {invalid_socket_descriptor, Descriptor}}
						end},
				{error, missing_webmachine_socket})),
		{WuiSocketIp, WuiSocketPort} = enforce_ok_1 (mosaic_generic_coders:application_env_get (wui_address, mosaic_node,
				{decode,
						fun
							({Ip, Port}) when is_list (Ip), is_integer (Port) -> {ok, {erlang:list_to_binary (Ip), Port}};
							(Descriptor) -> {error, {invalid_socket_descriptor, Descriptor}}
						end},
				{error, missing_wui_socket})),
		PortName = {spawn_executable, erlang:binary_to_list (WuiExecutable)},
		PortSettings = [
				stream, binary,
				{env, [
						{"mosaic_node_ip", erlang:binary_to_list (WebmachineSocketIp)},
						{"mosaic_node_port", erlang:integer_to_list (WebmachineSocketPort)},
						{"mosaic_node_wui_ip", erlang:binary_to_list (WuiSocketIp)},
						{"mosaic_node_wui_port", erlang:integer_to_list (WuiSocketPort)}]}],
		ok = case mosaic_node_sup:start_child_daemon_port (PortName, PortSettings, [{restart, transient}]) of
			{ok, _Port} ->
				ok;
			Error2 = {error, _Reason2} ->
				throw (Error2)
		end,
		ok
	catch
		throw : ok -> ok;
		throw : Error3 = {error, _Reason3} -> Error3
	end.


boot () ->
	RiakCoreStopIntercepterLoop = fun (Loop) ->
			receive
				{riak_core, stop, _Reason} ->
					Loop (Loop)
			end
	end,
	_ = erlang:spawn (
			fun () ->
				true = erlang:register (riak_core_stop_intercepter, erlang:self ()),
				RiakCoreStopIntercepterLoop (RiakCoreStopIntercepterLoop)
			end),
	mosaic_application_tools:boot (mosaic_node).
