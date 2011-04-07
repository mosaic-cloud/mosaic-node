
-module (mosaic_cluster_app).

-behaviour (application).

-export ([boot/0, start/2, stop/1]).

boot () ->
	% _ = appmon:start (),
	ok = boot (mosaic_cluster),
	mosaic_executor_vnode:start (),
	ok.

boot (App) ->
	case application:load (App) of
		ok ->
			ok;
		{error, {already_loaded, App}} ->
			ok
	end,
	{ok, DepApps} = application:get_key (App, applications),
	ok = lists:foreach (fun (DepApp) -> boot (DepApp) end, DepApps),
	case application:start (App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end,
	ok.

start (normal, void) ->
	case mosaic_cluster_sup:start_link () of
		{ok, Supervisor} ->
			ok = riak_core_node_watcher:node_up (),
			{ok, Supervisor, void};
		{error, Reason} ->
			{error, Reason}
	end;
start (normal, Arguments) ->
	{error, {invalid_start_arguments, Arguments}};
start (Type, _Arguments) ->
	{error, {invalid_start_type, Type}}.

stop (void) ->
	ok;
stop (State) ->
	{error, {invalid_state, State}}.
