
-module (mosaic_component_sup).

-behaviour (supervisor).


-export ([start_link/0]).
-export ([init/1]).


start_link () ->
	supervisor:start_link (mosaic_component_sup, [defaults]).


init ([defaults]) ->
	{ok, {{one_for_one, 1, 60}, [
				{mosaic_component_callbacks, {mosaic_component_callbacks, start_link, []}, temporary, 60, worker, dynamic},
				{mosaic_component_backend, {mosaic_component_backend, start_link, []}, temporary, 60, worker, dynamic}]}}.
