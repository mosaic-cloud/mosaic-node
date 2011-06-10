
-module (mosaic_dummy_sup).

-behaviour (supervisor).


-export ([start_link/0]).
-export ([init/1]).


start_link () ->
	supervisor:start_link (mosaic_dummy_sup, [defaults]).


init ([defaults]) ->
	{ok, {{one_for_all, 1, 3600}, []}}.
