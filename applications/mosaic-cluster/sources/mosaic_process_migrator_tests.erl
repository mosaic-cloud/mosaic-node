
-module (mosaic_process_migrator_tests).

-export ([test/0]).
-export ([
		test_migrate/1]).
-export ([
		start_link_process_migrator/6, start_link_process_migrator/7,
		wait_process_migrator/1]).

-import (mosaic_process_tests, [start_link_process/2, stop_and_wait_process/1, wait_process/1]).


-test ({test_migrate, [defaults]}).


test_migrate (defaults) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	SourceToken = erlang:make_ref (),
	TargetToken = erlang:make_ref (),
	{ok, Source} = start_link_process (mosaic_process_tester, create),
	{ok, Target} = start_link_process (mosaic_process_tester, {migrate, TargetToken}),
	{ok, Migrator} = start_link_process_migrator (Source, SourceToken, Target, TargetToken, Self, SelfToken),
	ok = mosaic_process_migrator:migrate (Migrator, defaults),
	ok = wait_process_migrator (Migrator),
	ok = wait_process (Source),
	ok = stop_and_wait_process (Target),
	ok.


start_link_process_migrator (Source, SourceToken, Target, TargetToken, Observer, ObserverToken) ->
	start_link_process_migrator (noname, Source, SourceToken, Target, TargetToken, Observer, ObserverToken).

start_link_process_migrator (QualifiedName, Source, SourceToken, Target, TargetToken, Observer, ObserverToken) ->
	mosaic_tests:enforce_start_outcome (QualifiedName, mosaic_process_migrator:start_link (QualifiedName, Source, SourceToken, Target, TargetToken, Observer, ObserverToken)).

wait_process_migrator (Migrator) ->
	mosaic_tests:enforce_stop_outcome_and_wait (Migrator, [normal], ok).


test () ->
	mosaic_tests:test_module (mosaic_process_migrator_tests).
