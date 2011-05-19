
-module (mosaic_executor_tests).

-export ([test/0]).
-export ([
		test_ping/1,
		test_define_process/1,
		test_create_stop_process/1]).

-test ({test_ping, [{defaults}]}).
-test ({test_define_process, [{defaults}]}).
-test ({test_create_stop_process, [{defaults}]}).


test () ->
	ok = mosaic_cluster:boot (),
	ok = mosaic_cluster:node_activate (),
	ok = mosaic_executor:service_activate (),
	ok = mosaic_tests:test_module (mosaic_executor_tests),
	ok.


test_ping ({defaults}) ->
	ok = case mosaic_executor:ping () of
		{ok, _Pongs, _Pangs = []} ->
			ok;
		{ok, Pongs = [], Pangs} ->
			ok = mosaic_tools:trace_warning ("Ping failed completely!", [{pongs, Pongs}, {pangs, Pangs}]),
			ok;
		{ok, Pongs, Pangs} ->
			ok = mosaic_tools:trace_warning ("Ping failed partially!", [{pongs, Pongs}, {pangs, Pangs}]),
			ok
	end,
	ok.


test_define_process ({defaults}) ->
	{ok, _Key} = mosaic_executor:define_process (dummy, term, defaults),
	ok.


test_create_stop_process ({defaults}) ->
	{ok, Key} = mosaic_executor:define_process (dummy, term, defaults),
	{ok, _Process} = mosaic_executor:create_process (Key),
	ok = mosaic_executor:stop_process (Key),
	ok.
