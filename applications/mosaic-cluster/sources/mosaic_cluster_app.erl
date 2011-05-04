
-module (mosaic_cluster_app).

-behaviour (application).

-export ([start/2, stop/1]).


start (normal, defaults) ->
	try begin
		{ok, Supervisor} = start_supervisor (),
		ok = start_discovery_events (),
		ok = start_discovery_agent (),
		ok = start_webmachine (),
		ok = start_services (),
		{ok, Supervisor, void}
	end catch
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


start_discovery_events () ->
	ok = case mosaic_discovery_events:start_supervised () of
		{ok, _Events} ->
			ok;
		Error = {error, _Reason} ->
			throw (Error)
	end,
	ok.


start_discovery_agent () ->
	JoinFunction = fun (Message, void) ->
			case Message of
				{broadcasted, {mosaic_cluster, {join, Node}}} when is_atom (Node) ->
					% ok = mosaic_tools:report_info (mosaic_cluster_tests, test, join, {Node}),
					ok = mosaic_cluster:ring_include (Node),
					{ok, void};
				_ ->
					{ok, void}
			end
	end,
	ok = case mosaic_discovery_agent:start_supervised () of
		{ok, _Agent} ->
			ok;
		Error1 = {error, _Reason1} ->
			throw (Error1)
	end,
	ok = case mosaic_discovery_events:register_handler (mosaic_discovery_events, {JoinFunction, void}) of
		ok ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end,
	ok = case mosaic_discovery_agent:broadcast ({mosaic_cluster, {join, erlang:node ()}}) of
		ok ->
			ok;
		Error4 = {error, _Reason4} ->
			throw (Error4)
	end,
	ok.


start_webmachine () ->
	case mosaic_webmachine:start_supervised () of
		{ok, _Webmachine} ->
			ok;
		Error = {error, _Reason} ->
			throw (Error)
	end.


start_services () ->
	ok = case mosaic_process_configurator:start_supervised () of
		{ok, _Configurator} ->
			ok;
		Error1 = {error, _Reason1} ->
			throw (Error1)
	end,
	ok = case mosaic_executor:service_activate () of
		ok ->
			ok;
		Error2 = {error, _Reason2} ->
			throw (Error2)
	end,
	ok = case mosaic_cluster:node_activate () of
		ok ->
			ok;
		Error3 = {error, _Reason3} ->
			throw (Error3)
	end,
	ok.
