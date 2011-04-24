
-module (mosaic_executor_tests).

-export ([test/0]).

-export ([ping/1]).
-test ({ping, [16]}).

-export ([define_process/2]).
-test ({define_process, [mosaic_process_tester, ok]}).

-export ([create_process/2]).
-test ({create_process, [mosaic_process_tester, ok]}).


test () ->
	ok = mosaic_cluster_app:boot (),
	ok = mosaic_tests:test_module (mosaic_executor_tests),
	ok.


ping (Count)
		when is_integer (Count), (Count > 0) ->
	case mosaic_executor:ping (Count) of
		{ok, Count, []} ->
			ok;
		{ok, 0, Outcomes} ->
			ok = error_logger:error_report ("Ping failed completely!", [Outcomes]),
			ok;
		{ok, PingCount, Outcomes} when (PingCount =< Count) ->
			ok = error_logger:warning_report ("Ping failed partially!", [Outcomes]),
			ok
	end.


define_process (Module, Arguments)
		when is_atom (Module) ->
	{ok, _Key} = mosaic_executor:define_process (Module, Arguments),
	ok.


create_process (Module, Arguments)
		when is_atom (Module) ->
	{ok, Key} = mosaic_executor:define_process (Module, Arguments),
	{ok, _Process} = mosaic_executor:create_process (Key),
	ok = mosaic_executor:stop_process (Key),
	ok.
