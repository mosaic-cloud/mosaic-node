
-module (mosaic_dummy_app).

-behaviour (application).


-export ([start/2, stop/1]).


start (normal, defaults) ->
	mosaic_dummy_sup:start_link ();
	
start (normal, Configuration) ->
	{error, {invalid_configuration, Configuration}};
	
start (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


stop (void) ->
	ok.
