
-module (mosaic_component_harness_tests).


-export ([test/0]).
-export ([
		test_start_stop/1,
		test_execute/1,
		test_signal/1,
		test_exchange/1]).


-test ({test_start_stop, [{defaults}]}).
-test ({test_execute, [{sleep, wait}]}).
%-test ({test_execute, [{sleep, nowait}]}).
-test ({test_execute, [{cat, nowait}]}).
-test ({test_signal, [{defaults}]}).
-test ({test_exchange, [{defaults}]}).


test_start_stop ({defaults}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Harness} = mosaic_component_harness:start_link ([{controller, Self}, {controller_token, SelfToken}]),
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test_execute ({sleep, wait}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Harness} = mosaic_component_harness:start_link ([{controller, Self}, {controller_token, SelfToken}]),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/sleep">>}, {arguments, [<<"0.1s">>]}]),
	ok = receive {mosaic_component_harness, exit, SelfToken, 0} -> ok end,
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok;
	
test_execute ({sleep, nowait}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Harness} = mosaic_component_harness:start_link ([{controller, Self}, {controller_token, SelfToken}]),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/sleep">>}, {arguments, [<<"1h">>]}]),
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok;
	
test_execute ({cat, nowait}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Harness} = mosaic_component_harness:start_link ([{controller, Self}, {controller_token, SelfToken}]),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/cat">>}, {arguments, []}]),
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test_signal ({defaults}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Harness} = mosaic_component_harness:start_link ([{controller, Self}, {controller_token, SelfToken}]),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/sleep">>}, {arguments, [<<"1h">>]}]),
	ok = mosaic_component_harness:signal (Harness, terminate),
	ok = receive {mosaic_component_harness, exit, SelfToken, 15} -> ok end,
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test_exchange ({defaults}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Harness} = mosaic_component_harness:start_link ([{controller, Self}, {controller_token, SelfToken}]),
	ok = mosaic_component_harness:execute (Harness, [{executable, <<"/bin/cat">>}]),
	ok = mosaic_component_harness:exchange (Harness, {[{attribute_1, value_1}, {attribute_2, value_2}], <<"data">>}),
	ok = receive {mosaic_component_harness, exchange, SelfToken, {MetaData, Data}} when is_list (MetaData), is_binary (Data) -> ok end,
	ok = mosaic_component_harness:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test () ->
	mosaic_tests:test_module (mosaic_component_harness_tests).
