
-module (mosaic_process_tests).

-export ([test/0]).


-define (process_tester__enabled, _).
-define (process_controller__enabled, _).
-define (process_migrator__enabled, _).
-define (port_process__enabled, _).


-export ([
		process_tester__start_stop/1,
		process_tester__ping/1,
		process_tester__start_error/1,
		process_tester__stop_error/1,
		process_tester__migrate_as_source/1,
		process_tester__migrate_as_target/1]).

-ifdef (process_tester__enabled).
-test ({process_tester__start_stop, [[{stop_method, stop}, {stop_signal, normal}]]}).
-test ({process_tester__start_stop, [[{stop_method, stop}, {stop_signal, reference}]]}).
-test ({process_tester__start_stop, [[{stop_method, call}, {stop_signal, normal}]]}).
-test ({process_tester__start_stop, [[{stop_method, cast}, {stop_signal, normal}]]}).
-test ({process_tester__start_stop, [[{stop_method, info}, {stop_signal, normal}]]}).
-test ({process_tester__start_error, [defaults]}).
-test ({process_tester__stop_error, [defaults]}).
-test ({process_tester__ping, [[{ping_method, call_reply}]]}).
-test ({process_tester__ping, [[{ping_method, call_noreply}]]}).
-test ({process_tester__ping, [[{ping_method, cast}]]}).
-test ({process_tester__ping, [[{ping_method, info}]]}).
-test ({process_tester__migrate_as_source, [reject]}).
-test ({process_tester__migrate_as_source, [terminate]}).
-test ({process_tester__migrate_as_source, [{commit, succeed}]}).
-test ({process_tester__migrate_as_source, [{commit, fail}]}).
-test ({process_tester__migrate_as_source, [{rollback, succeed}]}).
-test ({process_tester__migrate_as_source, [{rollback, fail}]}).
-test ({process_tester__migrate_as_target, [reject]}).
-test ({process_tester__migrate_as_target, [terminate]}).
-test ({process_tester__migrate_as_target, [{commit, succeed}]}).
-test ({process_tester__migrate_as_target, [{commit, fail}]}).
-test ({process_tester__migrate_as_target, [{rollback, succeed}]}).
-test ({process_tester__migrate_as_target, [{rollback, fail}]}).
-endif.


-export ([
		process_controller__start_stop/1,
		process_controller__start_error/1,
		process_controller__create/1,
		process_controller__migrate/1]).

-ifdef (process_controller__enabled).
-test ({process_controller__start_stop, [defaults]}).
-test ({process_controller__start_error, [defaults]}).
-test ({process_controller__create, [succeed]}).
-test ({process_controller__create, [{fail, module}]}).
-test ({process_controller__create, [{fail, failed}]}).
-test ({process_controller__migrate, [once]}).
-test ({process_controller__migrate, [twice]}).
-endif.


-export ([
		process_migrator__migrate/1]).

-ifdef (process_migrator__enabled).
-test ({process_migrator__migrate, [defaults]}).
-endif.


-export ([
		port_process__start_stop/1,
		port_process__migrate_as_source/1,
		port_process__migrate_as_target/1]).

-ifdef (port_process__enabled).
-test ({port_process__start_stop, [defaults]}).
-test ({port_process__migrate_as_source, [commit]}).
-test ({port_process__migrate_as_source, [rollback]}).
-test ({port_process__migrate_as_target, [commit]}).
-test ({port_process__migrate_as_target, [rollback]}).
-endif.


process_tester__start_stop ([{stop_method, StopMethod}, {stop_signal, reference}]) ->
	process_tester__start_stop ([{stop_method, StopMethod}, {stop_signal, erlang:make_ref ()}]);
	
process_tester__start_stop ([{stop_method, StopMethod}, {stop_signal, StopSignal}]) ->
	{ok, Process} = start_link_process (noname, mosaic_process_tester, {create, defaults}),
	ok = case StopMethod of
		stop ->
			ReplyToken = erlang:make_ref (),
			{ok, ReplyToken} = mosaic_process:stop (Process, {stop, StopSignal, {ok, ReplyToken}}),
			ok;
		call ->
			ReplyToken = erlang:make_ref (),
			{ok, ReplyToken} = mosaic_process:call (Process, {stop, StopSignal, {ok, ReplyToken}}),
			ok;
		cast ->
			ok = mosaic_process:cast (Process, {stop, StopSignal}),
			ok;
		info ->
			Process ! {stop, StopSignal},
			ok
	end,
	ok = join_process (Process, [StopSignal]),
	ok.

process_tester__start_error (defaults) ->
	StopToken = erlang:make_ref (),
	{error, StopToken} = start_link_process (noname, mosaic_process_tester, {create, {stop, StopToken}}),
	ok.

process_tester__stop_error (defaults) ->
	ReplyToken = erlang:make_ref (),
	{ok, Process} = start_link_process (noname, mosaic_process_tester, {create, defaults}),
	{error, ReplyToken} = mosaic_process:stop (Process, {reply, {error, ReplyToken}}),
	ok = stop_and_join_process (Process),
	ok.

process_tester__ping ([{ping_method, PingMethod}]) ->
	Self = erlang:self (),
	PingToken = erlang:make_ref (),
	{ok, Process} = start_link_process (noname, mosaic_process_tester, {create, defaults}),
	{ok, PongToken} = case PingMethod of
		call_reply ->
			{pong, PingToken, PongToken_} = mosaic_process:call (Process, {ping, Self, PingToken, reply}),
			{ok, PongToken_};
		call_noreply ->
			{pong, PingToken, PongToken_} = mosaic_process:call (Process, {ping, Self, PingToken, noreply}),
			{ok, PongToken_};
		cast ->
			ok = mosaic_process:cast (Process, {ping, Self, PingToken}),
			{ok, PingToken};
		info ->
			Process ! {ping, Self, PingToken},
			{ok, PingToken}
	end,
	ok = receive {pong, PingToken, PongToken} -> ok after 1000 -> receive_unexpected () end,
	ok = stop_and_join_process (Process),
	ok.

process_tester__migrate_as_source (Completion) ->
	MigrateArguments = erlang:make_ref (),
	Self = erlang:self (),
	SourceToken = erlang:make_ref (),
	{ok, Source} = start_link_process (noname, mosaic_process_tester, {create, defaults}),
	ok = case Completion of
		reject ->
			{error, rejected} = mosaic_process:begin_migration (Source, SourceToken, reject, Self),
			ok = stop_and_join_process (Source),
			ok;
		terminate ->
			{error, terminated} = mosaic_process:begin_migration (Source, SourceToken, terminate, Self),
			ok = join_process (Source, [{migration_failed, terminated}]),
			ok;
		{_, CompletionOutcome} ->
			ok = mosaic_process:begin_migration (Source, SourceToken, {continue, MigrateArguments, CompletionOutcome}, Self),
			ok = receive {begin_migration, SourceToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
			ok = receive {continue_migration, SourceToken, prepared, MigrateArguments} -> ok after 1000 -> receive_unexpected () end,
			ok = receive {continue_migration, SourceToken, completed} -> ok after 1000 -> receive_unexpected () end,
			case Completion of
				{commit, succeed} ->
					ok = mosaic_process:commit_migration (Source, SourceToken),
					ok = receive {commit_migration, SourceToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
					ok = join_process (Source),
					ok;
				{commit, fail} ->
					{error, failed} = mosaic_process:commit_migration (Source, SourceToken),
					ok = receive {commit_migration, SourceToken, failed, failed} -> ok after 1000 -> receive_unexpected () end,
					ok = join_process (Source, [{migration_failed, failed}]),
					ok;
				{rollback, succeed} ->
					ok = mosaic_process:rollback_migration (Source, SourceToken),
					ok = receive {rollback_migration, SourceToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
					ok = stop_and_join_process (Source),
					ok;
				{rollback, fail} ->
					{error, failed} = mosaic_process:rollback_migration (Source, SourceToken),
					ok = receive {rollback_migration, SourceToken, failed, failed} -> ok after 1000 -> receive_unexpected () end,
					ok = join_process (Source, [{migration_failed, failed}]),
					ok
			end
	end,
	ok.

process_tester__migrate_as_target (Completion) ->
	Self = erlang:self (),
	TargetToken = erlang:make_ref (),
	{ok, Target} = start_link_process (noname, mosaic_process_tester, {migrate, TargetToken}),
	ok = case Completion of
		reject ->
			{error, rejected} = mosaic_process:begin_migration (Target, TargetToken, reject, Self),
			ok = join_process (Target, [{migration_failed, rejected}]),
			ok;
		terminate ->
			{error, terminated} = mosaic_process:begin_migration (Target, TargetToken, terminate, Self),
			ok = join_process (Target, [{migration_failed, terminated}]),
			ok;
		{_, CompletionOutcome} ->
			ok = mosaic_process:begin_migration (Target, TargetToken, {continue, CompletionOutcome}, Self),
			ok = receive {begin_migration, TargetToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
			ok = receive {continue_migration, TargetToken, completed} -> ok after 1000 -> receive_unexpected () end,
			ok = case Completion of
				{commit, succeed} ->
					ok = mosaic_process:commit_migration (Target, TargetToken),
					ok = receive {commit_migration, TargetToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
					ok = stop_and_join_process (Target),
					ok;
				{commit, fail} ->
					{error, failed} = mosaic_process:commit_migration (Target, TargetToken),
					ok = receive {commit_migration, TargetToken, failed, failed} -> ok after 1000 -> receive_unexpected () end,
					ok = join_process (Target, [{migration_failed, failed}]),
					ok;
				{rollback, succeed} ->
					ok = mosaic_process:rollback_migration (Target, TargetToken),
					ok = receive {rollback_migration, TargetToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
					ok = join_process (Target),
					ok;
				{rollback, fail} ->
					{error, failed} = mosaic_process:rollback_migration (Target, TargetToken),
					ok = receive {rollback_migration, TargetToken, failed, failed} -> ok after 1000 -> receive_unexpected () end,
					ok = join_process (Target, [{migration_failed, failed}]),
					ok
			end
	end,
	ok.


process_controller__start_stop (defaults) ->
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, Controller} = start_link_process_controller (noname, defaults),
	ok = stop_and_join_process_controller (Controller),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:join (Supervisor),
	ok.

process_controller__start_error (defaults) ->
	{error, process_supervisor_does_not_exist} = mosaic_process_controller:start_link (noname, defaults),
	ok.

process_controller__create (Outcome) ->
	Key = erlang:make_ref (),
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, Controller} = mosaic_process_controller:start_link (noname, defaults),
	ok = case Outcome of
		succeed ->
			{ok, Process} = mosaic_process_controller:create (Controller, Key, mosaic_process_tester, defaults),
			ok = link_process (Process),
			ok = stop_and_join_process (Process),
			ok;
		{fail, failed} ->
			{error, failed} = mosaic_process_controller:create (Controller, Key, mosaic_process_tester, {stop, failed}),
			ok;
		{fail, module} ->
			{error, {undef, [{undefined_module, init, _} | _]}} = mosaic_process_controller:create (Controller, Key, undefined_module, undefined),
			ok
	end,
	ok = stop_and_join_process_controller (Controller),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:join (Supervisor),
	ok.

process_controller__migrate (once) ->
	Key = erlang:make_ref (),
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, SourceController} = start_link_process_controller (noname, defaults),
	{ok, TargetController} = start_link_process_controller (noname, defaults),
	{ok, Source} = mosaic_process_controller:create (SourceController, Key, mosaic_process_tester, defaults),
	ok = link_process (Source),
	{ok, Target} = mosaic_process_controller:migrate (SourceController, TargetController, Key),
	ok = link_process (Target),
	ok = join_process (Source),
	ok = stop_and_join_process (Target),
	ok = stop_and_join_process_controller (SourceController),
	ok = stop_and_join_process_controller (TargetController),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:join (Supervisor),
	ok;
	
process_controller__migrate (twice) ->
	Key = erlang:make_ref (),
	{ok, Supervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, SourceController} = start_link_process_controller (noname, defaults),
	{ok, TargetController} = start_link_process_controller (noname, defaults),
	{ok, Source1} = mosaic_process_controller:create (SourceController, Key, mosaic_process_tester, defaults),
	ok = link_process (Source1),
	{ok, Target1} = mosaic_process_controller:migrate (SourceController, TargetController, Key),
	ok = link_process (Target1),
	ok = join_process (Source1),
	Source2 = Target1,
	{ok, Target2} = mosaic_process_controller:migrate (TargetController, SourceController, Key),
	ok = link_process (Target2),
	ok = join_process (Source2),
	ok = stop_and_join_process (Target2),
	ok = stop_and_join_process_controller (SourceController),
	ok = stop_and_join_process_controller (TargetController),
	true = erlang:exit (Supervisor, normal),
	ok = mosaic_tests:join (Supervisor),
	ok.


process_migrator__migrate (defaults) ->
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	SourceToken = erlang:make_ref (),
	TargetToken = erlang:make_ref (),
	{ok, Source} = start_link_process (noname, mosaic_process_tester, {create, defaults}),
	{ok, Target} = start_link_process (noname, mosaic_process_tester, {migrate, TargetToken}),
	{ok, Migrator} = start_link_process_migrator (noname, Source, SourceToken, Target, TargetToken, Self, SelfToken),
	ok = mosaic_process_migrator:migrate (Migrator, defaults),
	ok = join_process_migrator (Migrator),
	ok = join_process (Source),
	ok = stop_and_join_process (Target),
	ok.


port_process__start_stop (defaults) ->
	CreateArguments = {{spawn_executable, "/bin/cat"}, []},
	{ok, Process} = start_link_process (noname, mosaic_port_process, {create, CreateArguments}),
	ok = stop_and_join_process (Process),
	ok.

port_process__migrate_as_source (CompletionMethod) ->
	CreateArguments = {{spawn_executable, "/bin/cat"}, []},
	Self = erlang:self (),
	SourceToken = erlang:make_ref (),
	{ok, Source} = start_link_process (noname, mosaic_port_process, {create, CreateArguments}),
	ok = mosaic_process:begin_migration (Source, SourceToken, defaults, Self),
	ok = receive {begin_migration, SourceToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
	ok = receive {continue_migration, SourceToken, prepared, CreateArguments} -> ok after 1000 -> receive_unexpected () end,
	ok = receive {continue_migration, SourceToken, completed} -> ok after 1000 -> receive_unexpected () end,
	ok = case CompletionMethod of
		commit ->
			ok = mosaic_process:commit_migration (Source, SourceToken),
			ok = receive {commit_migration, SourceToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
			ok = join_process (Source),
			ok;
		rollback ->
			ok = mosaic_process:rollback_migration (Source, SourceToken),
			ok = receive {rollback_migration, SourceToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
			ok = stop_and_join_process (Source),
			ok
	end,
	ok.

port_process__migrate_as_target (CompletionMethod) ->
	CreateArguments = {{spawn_executable, "/bin/cat"}, []},
	Self = erlang:self (),
	TargetToken = erlang:make_ref (),
	{ok, Target} = start_link_process (noname, mosaic_port_process, {migrate, TargetToken}),
	ok = mosaic_process:begin_migration (Target, TargetToken, CreateArguments, Self),
	ok = receive {begin_migration, TargetToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
	ok = receive {continue_migration, TargetToken, completed} -> ok after 1000 -> receive_unexpected () end,
	ok = case CompletionMethod of
		commit ->
			ok = mosaic_process:commit_migration (Target, TargetToken),
			ok = receive {commit_migration, TargetToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
			ok = stop_and_join_process (Target),
			ok;
		rollback ->
			ok = mosaic_process:rollback_migration (Target, TargetToken),
			ok = receive {rollback_migration, TargetToken, succeeded} -> ok after 1000 -> receive_unexpected () end,
			ok = join_process (Target),
			ok
	end,
	ok.


start_link_process (QualifiedName, Module, Disposition) ->
	enforce_start_outcome (QualifiedName, mosaic_process:start_link (QualifiedName, Module, Disposition)).

link_process (Process) ->
	true = erlang:link (Process),
	ok.

stop_and_join_process (Process) ->
	stop_and_join_process (Process, normal, [normal]).

stop_and_join_process (Process, Signal, Reasons) ->
	continue_stop_and_join (Process, Reasons, mosaic_process:stop (Process, Signal)).

join_process (Process) ->
	join_process (Process, [normal]).

join_process (Process, Reasons) ->
	continue_stop_and_join (Process, Reasons, ok).


start_link_process_controller (QualifiedName, Configuration) ->
	enforce_start_outcome (QualifiedName, mosaic_process_controller:start_link (QualifiedName, Configuration)).

stop_and_join_process_controller (Controller) ->
	stop_and_join_process_controller (Controller, normal, [normal]).

stop_and_join_process_controller (Controller, Signal, Reasons) ->
	continue_stop_and_join (Controller, Reasons, mosaic_process_controller:stop (Controller, Signal)).


start_link_process_migrator (QualifiedName, Source, SourceToken, Target, TargetToken, Observer, ObserverToken) ->
	enforce_start_outcome (QualifiedName, mosaic_process_migrator:start_link (QualifiedName, Source, SourceToken, Target, TargetToken, Observer, ObserverToken)).

join_process_migrator (Migrator) ->
	continue_stop_and_join (Migrator, [normal], ok).


enforce_start_outcome (QualifiedName, Outcome) ->
	case Outcome of
		{ok, Process} when is_pid (Process) ->
			case mosaic_tools:enforce_registered (QualifiedName, Process) of
				ok ->
					Outcome;
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


continue_stop_and_join (Process, Reasons, Outcome) ->
	case Outcome of
		ok ->
			case mosaic_tests:join (Process, Reasons) of
				{ok, _} ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


receive_unexpected () ->
	receive
		Message ->
			{unexpected_message, Message}
	after 0 ->
		timeout
	end.


test () ->
	mosaic_tests:test_module (mosaic_process_tests).
