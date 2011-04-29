
-module (mosaic_component_harness_tests).

-export ([test/0]).


-export ([test_start_stop/0, test_execute/0, test_signal/0]).

-test ({test_start_stop, []}).
-test ({test_execute, []}).
-test ({test_signal, []}).


test_start_stop () ->
	{ok, Harness} = mosaic_component_harness:start_link (),
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:join (Harness),
	ok.


test_execute () ->
	{ok, Harness} = mosaic_component_harness:start_link (),
	timer:sleep (500),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/cat">>}]),
	timer:sleep (500),
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:join (Harness).

test_signal () ->
	{ok, Harness} = mosaic_component_harness:start_link (),
	timer:sleep (500),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/cat">>}]),
	timer:sleep (500),
	ok = mosaic_component_harness:signal (Harness, terminate),
	timer:sleep (500),
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:join (Harness).

test () ->
	mosaic_tests:test_module (mosaic_component_harness_tests).
