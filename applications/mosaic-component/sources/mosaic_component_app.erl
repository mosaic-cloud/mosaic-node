
-module (mosaic_component_app).

-behaviour (application).


-export ([start/2, stop/1, boot/0, boot/2]).


-import (mosaic_enforcements, [enforce/1, enforce/2]).


start (normal, defaults) ->
	mosaic_component_sup:start_link ();
	
start (normal, Configuration) ->
	{error, {invalid_configuration, Configuration}};
	
start (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


stop (void) ->
	ok.


boot () ->
	boot (mosaic_component, start).


boot (mosaic_component, load) ->
	try
		ok = enforce (application:load (mosaic_component),
				[{{error, {already_loaded, mosaic_component}}, ok}])
	catch throw : Error = {error, _} -> Error end;
	
boot (mosaic_component, start) ->
	try
		ok = enforce (boot (mosaic_component, load)),
		{ok, Handler} = enforce (application:get_env (mosaic_component, handler),
				[{undefined, {error, {invalid_environment, missing_handler}}}]),
		if
			is_atom (Handler) -> ok;
			true -> throw ({error, {invalid_environment, {invalid_handler, Handler}}})
		end,
		ok = enforce (Handler:configure ()),
		ok = enforce (boot (mosaic_component, start))
	catch throw : Error = {error, _} -> Error end.
