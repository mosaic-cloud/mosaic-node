
-module (mosaic_node_app).

-behaviour (application).


-export ([start/2, stop/1, boot/0]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


start (normal, defaults) ->
	try
		{ok, Supervisor} = start_supervisor (),
		ok = start_discovery (),
		ok = start_daemons (),
		ok = start_wui (),
		ok = report (),
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
	ok = case mosaic_discovery_agent:start_supervised () of
		{ok, _Agent} ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end,
	ok = case mosaic_discovery_events:register_handler (mosaic_discovery_events, {JoinFunction, void}) of
		ok ->
			ok;
		Error3 = {error, _Reason3} ->
			throw (Error3)
	end,
	ok = case mosaic_discovery_agent:broadcast ({mosaic_node, node, erlang:node ()}) of
		{ok, _Reference} ->
			ok;
		Error4 = {error, _Reason4} ->
			throw (Error4)
	end,
	ok.


start_wui () ->
	try
		WuiExecutable = try
			enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-node-wui--run-node-wui">>))
		catch throw : _Error1 = {error, _Reason1} -> throw (ok) end,
		{WebmachineSocketIp, WebmachineSocketPort} = enforce_ok_1 (mosaic_generic_coders:application_env_get (webmachine_listen, mosaic_node,
				{decode,
						fun
							({Ip, Port}) when is_list (Ip), is_integer (Port) -> {ok, {erlang:list_to_binary (Ip), Port}};
							(Descriptor) -> {error, {invalid_socket_descriptor, Descriptor}}
						end},
				{error, missing_webmachine_socket})),
		{WuiSocketIp, WuiSocketPort} = enforce_ok_1 (mosaic_generic_coders:application_env_get (wui_listen, mosaic_node,
				{decode,
						fun
							({Ip, Port}) when is_list (Ip), is_integer (Port) -> {ok, {erlang:list_to_binary (Ip), Port}};
							(Descriptor) -> {error, {invalid_socket_descriptor, Descriptor}}
						end},
				{error, missing_wui_socket})),
		_Port = erlang:open_port ({spawn_executable, erlang:binary_to_list (WuiExecutable)}, [
				stream, binary,
				{env, [
						{"mosaic_node_ip", erlang:binary_to_list (WebmachineSocketIp)},
						{"mosaic_node_port", erlang:integer_to_list (WebmachineSocketPort)},
						{"mosaic_node_wui_ip", erlang:binary_to_list (WuiSocketIp)},
						{"mosaic_node_wui_port", erlang:integer_to_list (WuiSocketPort)}]}]),
		ok
	catch
		throw : ok -> ok;
		throw : Error2 = {error, _Reason2} -> Error2
	end.


report () ->
	try
		{WebmachineSocketIp, WebmachineSocketPort} = enforce_ok_1 (mosaic_generic_coders:application_env_get (webmachine_listen, mosaic_node,
				{decode,
						fun
							({Ip, Port}) when is_list (Ip), is_integer (Port) -> {ok, {erlang:list_to_binary (Ip), Port}};
							(Descriptor) -> {error, {invalid_socket_descriptor, Descriptor}}
						end},
				{error, missing_webmachine_socket})),
		WebmachineSocketFqdn = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"mosaic_node_fqdn">>,
				{decode, fun mosaic_generic_coders:decode_string/1}, {error, missing_webmachine_fqdn})),
		WebmachineSocketFqdnString = erlang:binary_to_list (WebmachineSocketFqdn),
		WebmachineSocket = {WebmachineSocketIp, WebmachineSocketPort, WebmachineSocketFqdn},
		ok = error_logger:info_report (["Configured mOSAIC node webmachine...",
					{url, erlang:list_to_binary ("http://" ++ WebmachineSocketFqdnString ++ ":" ++ erlang:integer_to_list (WebmachineSocketPort) ++ "/")},
					{webmachine_endpoint, WebmachineSocket}]),
		WuiExecutable = try
			enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-node-wui--run-node-wui">>))
		catch throw : _Error1 = {error, _Reason1} -> undefined end,
		{WuiSocketIp, WuiSocketPort} = enforce_ok_1 (mosaic_generic_coders:application_env_get (wui_listen, mosaic_node,
				{decode,
						fun
							({Ip, Port}) when is_list (Ip), is_integer (Port) -> {ok, {erlang:list_to_binary (Ip), Port}};
							(Descriptor) -> {error, {invalid_socket_descriptor, Descriptor}}
						end},
				{error, missing_wui_socket})),
		WuiSocketFqdn = if
			WebmachineSocketIp =:= WuiSocketIp ->
				WebmachineSocketFqdn;
			true ->
				enforce_ok_1 (mosaic_generic_coders:decode_string (WuiSocketIp))
		end,
		WuiSocketFqdnString = erlang:binary_to_list (WuiSocketFqdn),
		WuiSocket = {WuiSocketIp, WuiSocketPort, WuiSocketFqdn},
		if
			WuiExecutable =/= undefined ->
				ok = error_logger:info_report (["Configured mOSAIC node web-ui...",
							{url, erlang:list_to_binary ("http://" ++ WuiSocketFqdnString ++ ":" ++ erlang:integer_to_list (WuiSocketPort) ++ "/")},
							{wui_endpoint, WuiSocket}]);
			true ->
				ok
		end,
		ok
	catch throw : Error = {error, _Reason} -> Error end.


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
