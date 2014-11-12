
-module (mosaic_component_app).

-behaviour (application).


-export ([start/2, stop/1, boot/0, standalone/0]).


-import (mosaic_enforcements, [enforce_ok/1]).


start (normal, defaults) ->
	mosaic_component_sup:start_link ();
	
start (normal, Configuration) ->
	{error, {invalid_configuration, Configuration}};
	
start (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


stop (void) ->
	ok.


boot () ->
	mosaic_application_tools:boot (
				fun () ->
					try
						ok = enforce_ok (mosaic_application_tools:load (mosaic_component)),
						ok = enforce_ok (mosaic_component_backend:configure ()),
						ok = enforce_ok (mosaic_application_tools:start (mosaic_component)),
						ok
					catch throw : Error = {error, _Reason} -> Error end
				end).


standalone () ->
	mosaic_application_tools:boot (
				fun () ->
					try
						ok = enforce_ok (mosaic_application_tools:load (mosaic_component)),
						ok = enforce_ok (mosaic_component_backend:standalone ()),
						ok
					catch throw : Error = {error, _Reason} -> Error end
				end).
