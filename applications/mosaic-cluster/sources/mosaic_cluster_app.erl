
-module (mosaic_cluster_app).

-behaviour (application).


-export ([start/2, stop/1, boot/0]).


start (normal, defaults) ->
	try
		{ok, Supervisor} = start_supervisor (),
		ok = start_discovery (),
		ok = start_daemons (),
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
	case mosaic_cluster_sup:start_link () of
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
				{mosaic_discovery_events, broadcasted, {mosaic_cluster, node, Node}} when is_atom (Node) ->
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
	ok = case mosaic_discovery_agent:broadcast ({mosaic_cluster, node, erlang:node ()}) of
		{ok, _Reference} ->
			ok;
		Error4 = {error, _Reason4} ->
			throw (Error4)
	end,
	ok.


boot () ->
	mosaic_application_tools:boot (mosaic_cluster).
