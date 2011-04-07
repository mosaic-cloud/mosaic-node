
-module (mosaic_cluster_sup).

-behaviour (supervisor).

-export ([start_link/0, init/1]).

start_link () ->
	supervisor:start_link ({local, mosaic_cluster_sup}, mosaic_cluster_sup, void).

init (void) ->
	{ok, {{one_for_one, 5, 10}, []}}.
