
-module (mosaic_cluster_app).

-behaviour (application).

-export ([start/2, stop/1]).


start (normal, defaults) ->
	case mosaic_cluster_sup:start_link () of
		{ok, Supervisor} ->
			ok = mosaic_webmachine:enforce_start (),
			{ok, Supervisor, void};
		Error = {error, _Reason} ->
			Error
	end;
	
start (normal, Arguments) ->
	{error, {invalid_start_arguments, Arguments}};
	
start (Type, _Arguments) ->
	{error, {invalid_start_type, Type}}.


stop (void) ->
	ok.
