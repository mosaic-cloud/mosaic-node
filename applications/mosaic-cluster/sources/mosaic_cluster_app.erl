
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
	case mosaic_sup:start_link () of
		Outcome = {ok, _Supervisor} ->
			Outcome;
		Error = {error, _Reason} ->
			throw (Error)
	end.


start_daemons () ->
	ok = case mosaic_cluster_processes_configurator:start_supervised () of
		{ok, _Configurator} ->
			ok;
		Error1 = {error, _Reason1} ->
			throw (Error1)
	end,
	ok = case mosaic_cluster_processes_router:start_supervised () of
		{ok, _Router} ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end,
	ok = case mosaic_cluster_resources:start_supervised () of
		{ok, _Resources} ->
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
					ok = mosaic_cluster_tools:ring_include (Node),
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
	boot (mosaic_cluster).

boot ([]) ->
	ok;
	
boot ([App | RemainingApps])
		when is_atom (App), is_list (RemainingApps) ->
	case boot (App) of
		ok ->
			boot (RemainingApps);
		Error = {error, _Reason} ->
			Error
	end;
	
boot (App)
		when is_atom (App) ->
	ok = case application:load (App) of
		ok ->
			ok;
		{error, {already_loaded, App}} ->
			ok
	end,
	case application:get_key (App, applications) of
		{ok, DepApps} ->
			case boot (DepApps) of
				ok ->
					case application:start (App) of
						ok ->
							ok;
						{error, {already_started, App}} ->
							ok;
						Error = {error, _Reason} ->
							Error
					end;
				Error = {error, _Reason} ->
					Error
			end;
		undefined ->
			{error, {undefined_dependencies, App}}
	end.
