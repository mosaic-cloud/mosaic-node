
-module (mosaic_process_controller_tests).


-export ([test/0]).
-export ([
		test_start_stop/1,
		test_create/1,
		test_migrate/1]).
-export ([
		start_link_process_controller/0, start_link_process_controller/1, start_link_process_controller/2,
		stop_and_wait_process_controller/1, stop_and_wait_process_controller/3]).


-import (mosaic_process_tests, [stop_and_wait_process/1, wait_process/1]).


-test ({test_start_stop, [{defaults}]}).
-test ({test_create, [{succeed}]}).
-test ({test_create, [{fail, module}]}).
-test ({test_create, [{fail, failed}]}).
-test ({test_migrate, [{once}]}).
-test ({test_migrate, [{twice}]}).


test_start_stop ({defaults}) ->
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, Controller} = start_link_process_controller (),
	ok = stop_and_wait_process_controller (Controller),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:wait (Supervisor),
	ok.


test_create (Outcome) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, Controller} = mosaic_process_controller:start_link (),
	ok = case Outcome of
		{succeed} ->
			{ok, Process} = mosaic_process_controller:create (Controller, Identifier, mosaic_process_tester, defaults),
			true = erlang:link (Process),
			ok = stop_and_wait_process (Process),
			ok;
		{fail, failed} ->
			{error, failed} = mosaic_process_controller:create (Controller, Identifier, mosaic_process_tester, {stop, failed}),
			ok;
		{fail, module} ->
			{error, {undef, [{undefined_module, init, _} | _]}} = mosaic_process_controller:create (Controller, Identifier, undefined_module, undefined_configuration),
			ok
	end,
	ok = stop_and_wait_process_controller (Controller),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:wait (Supervisor),
	ok.


test_migrate ({once}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, SourceController} = start_link_process_controller (),
	{ok, TargetController} = start_link_process_controller (),
	{ok, Source} = mosaic_process_controller:create (SourceController, Identifier, mosaic_process_tester, defaults),
	true = erlang:link (Source),
	ok = mosaic_process_controller:migrate (SourceController, TargetController, Identifier, defaults, mosaic_process_tester, defaults),
	ok = wait_process (Source),
	{ok, Target} = mosaic_process_controller:resolve (TargetController, Identifier),
	true = erlang:link (Target),
	ok = stop_and_wait_process (Target),
	ok = stop_and_wait_process_controller (SourceController),
	ok = stop_and_wait_process_controller (TargetController),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:wait (Supervisor),
	ok;
	
test_migrate ({twice}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, SourceController} = start_link_process_controller (),
	{ok, TargetController} = start_link_process_controller (),
	{ok, Source1} = mosaic_process_controller:create (SourceController, Identifier, mosaic_process_tester, defaults),
	true = erlang:link (Source1),
	ok = mosaic_process_controller:migrate (SourceController, TargetController, Identifier, defaults, mosaic_process_tester, defaults),
	ok = wait_process (Source1),
	{ok, Target1} = mosaic_process_controller:resolve (TargetController, Identifier),
	true = erlang:link (Target1),
	Source2 = Target1,
	ok = mosaic_process_controller:migrate (TargetController, SourceController, Identifier, defaults, mosaic_process_tester, defaults),
	ok = wait_process (Source2),
	{ok, Target2} = mosaic_process_controller:resolve (SourceController, Identifier),
	true = erlang:link (Target2),
	ok = stop_and_wait_process (Target2),
	ok = stop_and_wait_process_controller (SourceController),
	ok = stop_and_wait_process_controller (TargetController),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:wait (Supervisor),
	ok.


start_link_process_controller () ->
	start_link_process_controller (defaults).

start_link_process_controller (Configuration) ->
	start_link_process_controller (noname, Configuration).

start_link_process_controller (QualifiedName, Configuration) ->
	mosaic_tests:enforce_start_outcome (QualifiedName, mosaic_process_controller:start_link (QualifiedName, Configuration)).

stop_and_wait_process_controller (Controller) ->
	stop_and_wait_process_controller (Controller, normal, [normal]).

stop_and_wait_process_controller (Controller, Signal, Reasons) ->
	mosaic_tests:enforce_stop_outcome_and_wait (Controller, Reasons, mosaic_process_controller:stop (Controller, Signal)).


test () ->
	mosaic_tests:test_module (mosaic_process_controller_tests).
