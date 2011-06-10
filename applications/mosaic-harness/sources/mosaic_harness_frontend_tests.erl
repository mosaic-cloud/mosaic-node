
-module (mosaic_harness_frontend_tests).


-export ([test/0]).
-export ([
		test_start_stop/1,
		test_execute/1,
		test_signal/1,
		test_push_packet/1]).


-test ({test_start_stop, [{defaults}]}).
-test ({test_execute, [{sleep, wait}]}).
%-test ({test_execute, [{sleep, nowait}]}).
-test ({test_execute, [{cat, nowait}]}).
-test ({test_signal, [{defaults}]}).
-test ({test_push_packet, [{defaults}]}).


test_start_stop ({defaults}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Configuration} = mosaic_harness_coders:decode_configuration (term, [{controller, Self}, {controller_token, SelfToken}]),
	{ok, Harness} = mosaic_harness_frontend:start_link (Configuration),
	ok = mosaic_harness_frontend:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test_execute ({sleep, wait}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Configuration} = mosaic_harness_coders:decode_configuration (term, [{controller, Self}, {controller_token, SelfToken}]),
	{ok, ExecuteSpecification} = mosaic_harness_coders:decode_execute_specification (term, [{executable, <<"/bin/sleep">>}, {arguments, [<<"0.1s">>]}]),
	{ok, Harness} = mosaic_harness_frontend:start_link (Configuration),
	ok = mosaic_harness_frontend:execute (Harness, ExecuteSpecification),
	ok = receive {mosaic_harness_frontend, SelfToken, exit, 0} -> ok end,
	ok = mosaic_harness_frontend:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok;
	
test_execute ({sleep, nowait}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Configuration} = mosaic_harness_coders:decode_configuration (term, [{controller, Self}, {controller_token, SelfToken}]),
	{ok, ExecuteSpecification} = mosaic_harness_coders:decode_execute_specification (term, [{executable, <<"/bin/sleep">>}, {arguments, [<<"1h">>]}]),
	{ok, Harness} = mosaic_harness_frontend:start_link (Configuration),
	ok = mosaic_harness_frontend:execute (Harness, ExecuteSpecification),
	ok = mosaic_harness_frontend:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok;
	
test_execute ({cat, nowait}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Configuration} = mosaic_harness_coders:decode_configuration (term, [{controller, Self}, {controller_token, SelfToken}]),
	{ok, ExecuteSpecification} = mosaic_harness_coders:decode_execute_specification (term, [{executable, <<"/bin/cat">>}, {arguments, []}]),
	{ok, Harness} = mosaic_harness_frontend:start_link (Configuration),
	ok = mosaic_harness_frontend:execute (Harness, ExecuteSpecification),
	ok = mosaic_harness_frontend:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test_signal ({defaults}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Configuration} = mosaic_harness_coders:decode_configuration (term, [{controller, Self}, {controller_token, SelfToken}]),
	{ok, ExecuteSpecification} = mosaic_harness_coders:decode_execute_specification (term, [{executable, <<"/bin/sleep">>}, {arguments, [<<"1h">>]}]),
	{ok, SignalSpecification} = mosaic_harness_coders:decode_signal_specification (term, terminate),
	{ok, Harness} = mosaic_harness_frontend:start_link (Configuration),
	ok = mosaic_harness_frontend:execute (Harness, ExecuteSpecification),
	ok = mosaic_harness_frontend:signal (Harness, SignalSpecification),
	ok = receive {mosaic_harness_frontend, SelfToken, exit, 15} -> ok end,
	ok = mosaic_harness_frontend:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test_push_packet ({defaults}) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Configuration} = mosaic_harness_coders:decode_configuration (term, [{controller, Self}, {controller_token, SelfToken}]),
	{ok, ExecuteSpecification} = mosaic_harness_coders:decode_execute_specification (term, [{executable, <<"/bin/cat">>}]),
	{ok, Harness} = mosaic_harness_frontend:start_link (Configuration),
	ok = mosaic_harness_frontend:execute (Harness, ExecuteSpecification),
	{ok, MetaDataName1} = mosaic_generic_coders:generate_hex_data (8),
	{ok, MetaDataValue1} = mosaic_generic_coders:generate_hex_data (8),
	MetaData = [{MetaDataName1, MetaDataValue1}],
	{ok, Data} = mosaic_generic_coders:generate_data (8),
	ok = mosaic_harness_frontend:push_packet (Harness, {exchange, MetaData, Data}),
	ok = receive {mosaic_harness_frontend, SelfToken, push_packet, {exchange, MetaData, Data}} -> ok end,
	ok = mosaic_harness_frontend:stop (Harness),
	ok = mosaic_tests:wait (Harness),
	ok.


test () ->
	mosaic_tests:test_module (mosaic_harness_frontend_tests).
