
-module (mosaic_webmachine).

-export ([start_link/2, enforce_start/0]).

start_link (QualifiedName = {local, LocalName}, Options)
		when is_atom (LocalName), is_list (Options) ->
	case webmachine_mochiweb:start ([{name, QualifiedName} | Options]) of
		Outcome = {ok, Server} when is_pid (Server) ->
			true = erlang:link (Server),
			Outcome;
		Error = {error, _Reason} ->
			Error
	end.

enforce_start () ->
	QualifiedName = {local, mosaic_webmachine},
	Options = [
			{ip, "127.0.0.1"},
			{port, 9999},
			{dispatch, []},
			{error_handler, webmachine_error_handler},
			{enable_perf_logger, false},
			{log_dir, "/tmp/webmachine"}],
	{ok, _Server} = mosaic_cluster_sup:start_child_daemon (QualifiedName, mosaic_webmachine, [Options], permanent),
	ok.
