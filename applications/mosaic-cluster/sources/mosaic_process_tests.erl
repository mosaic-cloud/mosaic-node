
-module (mosaic_process_tests).


-export ([test/0]).
-export ([
		test_start_stop/1,
		test_start_error/1,
		test_stop_error/1,
		test_ping/1,
		test_migrate_as_source/1,
		test_migrate_as_target/1]).
-export ([
		start_link_process/2, start_link_process/3, start_link_process/4, start_link_process/5,
		stop_process/1, stop_process/2,
		stop_and_wait_process/1, stop_and_wait_process/3,
		wait_process/1, wait_process/2,
		call_process/2, call_process/3,
		cast_process/2, cast_process/3]).
-export ([configure/6]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


-test ({test_start_stop, [[{stop_method, stop}, {stop_signal, normal}]]}).
-test ({test_start_stop, [[{stop_method, stop}, {stop_signal, reference}]]}).
-test ({test_start_stop, [[{stop_method, call}, {stop_signal, normal}]]}).
-test ({test_start_stop, [[{stop_method, cast}, {stop_signal, normal}]]}).
-test ({test_start_stop, [[{stop_method, info}, {stop_signal, normal}]]}).
-test ({test_start_error, [{defaults}]}).
-test ({test_stop_error, [{defaults}]}).
-test ({test_ping, [[{ping_method, call_reply}]]}).
-test ({test_ping, [[{ping_method, call_noreply}]]}).
-test ({test_ping, [[{ping_method, cast}]]}).
-test ({test_ping, [[{ping_method, info}]]}).
-test ({test_migrate_as_source, [{reject}]}).
-test ({test_migrate_as_source, [{terminate}]}).
-test ({test_migrate_as_source, [{commit, succeed}]}).
-test ({test_migrate_as_source, [{commit, fail}]}).
-test ({test_migrate_as_source, [{rollback, succeed}]}).
-test ({test_migrate_as_source, [{rollback, fail}]}).
-test ({test_migrate_as_target, [{reject}]}).
-test ({test_migrate_as_target, [{terminate}]}).
-test ({test_migrate_as_target, [{commit, succeed}]}).
-test ({test_migrate_as_target, [{commit, fail}]}).
-test ({test_migrate_as_target, [{rollback, succeed}]}).
-test ({test_migrate_as_target, [{rollback, fail}]}).


test_start_stop ([{stop_method, StopMethod}, {stop_signal, reference}]) ->
	test_start_stop ([{stop_method, StopMethod}, {stop_signal, erlang:make_ref ()}]);
	
test_start_stop ([{stop_method, StopMethod}, {stop_signal, StopSignal}]) ->
	{ok, Process} = start_link_process (mosaic_process_tester, create),
	ok = case StopMethod of
		stop ->
			ReplyToken = erlang:make_ref (),
			{ok, ReplyToken} = stop_process (Process, {stop, StopSignal, {ok, ReplyToken}}),
			ok;
		call ->
			ReplyToken = erlang:make_ref (),
			{ok, ReplyToken, <<>>} = call_process (Process, {stop, StopSignal, {ok, ReplyToken, <<>>}}),
			ok;
		cast ->
			ok = cast_process (Process, {stop, StopSignal}),
			ok;
		info ->
			Process ! {stop, StopSignal},
			ok
	end,
	ok = wait_process (Process, [StopSignal]),
	ok.


test_start_error ({defaults}) ->
	StopToken = erlang:make_ref (),
	{error, StopToken} = start_link_process (mosaic_process_tester, create, {stop, StopToken}),
	ok.


test_stop_error ({defaults}) ->
	ReplyToken = erlang:make_ref (),
	{ok, Process} = start_link_process (mosaic_process_tester, create),
	{error, ReplyToken} = stop_process (Process, {reply, {error, ReplyToken}}),
	ok = stop_and_wait_process (Process),
	ok.


test_ping ([{ping_method, PingMethod}]) ->
	Self = erlang:self (),
	PingToken = erlang:make_ref (),
	{ok, Process} = start_link_process (mosaic_process_tester, create),
	{ok, PongMethod, PongToken} = case PingMethod of
		call_reply ->
			{ok, {pong, PingToken, PongToken_}, <<>>} = call_process (Process, {ping, Self, PingToken, reply}),
			{ok, call, PongToken_};
		call_noreply ->
			{ok, {pong, PingToken, PongToken_}, <<>>} = call_process (Process, {ping, Self, PingToken, noreply}),
			{ok, call, PongToken_};
		cast ->
			ok = cast_process (Process, {ping, Self, PingToken}),
			{ok, cast, PingToken};
		info ->
			Process ! {ping, Self, PingToken},
			{ok, info, PingToken}
	end,
	ok = receive {pong, PongMethod, PingToken, PongToken} -> ok end,
	ok = stop_and_wait_process (Process),
	ok.


test_migrate_as_source (Completion) ->
	MigrateConfiguration = erlang:make_ref (),
	Self = erlang:self (),
	SourceToken = erlang:make_ref (),
	{ok, Source} = start_link_process (mosaic_process_tester, create),
	ok = case Completion of
		{reject} ->
			{error, rejected} = mosaic_process:begin_migration (Source, SourceToken, reject, Self),
			ok = stop_and_wait_process (Source),
			ok;
		{terminate} ->
			{error, terminated} = mosaic_process:begin_migration (Source, SourceToken, terminate, Self),
			ok = wait_process (Source, [{migration_failed, terminated}]),
			ok;
		{_, CompletionOutcome} ->
			ok = mosaic_process:begin_migration (Source, SourceToken, {continue, MigrateConfiguration, CompletionOutcome}, Self),
			ok = receive {mosaic_process_migrator, 'begin', SourceToken, succeeded} -> ok end,
			ok = receive {mosaic_process_migrator, continue, SourceToken, {prepared, MigrateConfiguration}} -> ok end,
			ok = receive {mosaic_process_migrator, continue, SourceToken, completed} -> ok end,
			case Completion of
				{commit, succeed} ->
					ok = mosaic_process:commit_migration (Source, SourceToken),
					ok = receive {mosaic_process_migrator, commit, SourceToken, succeeded} -> ok end,
					ok = wait_process (Source),
					ok;
				{commit, fail} ->
					{error, failed} = mosaic_process:commit_migration (Source, SourceToken),
					ok = receive {mosaic_process_migrator, commit, SourceToken, failed, failed} -> ok end,
					ok = wait_process (Source, [{migration_failed, failed}]),
					ok;
				{rollback, succeed} ->
					ok = mosaic_process:rollback_migration (Source, SourceToken),
					ok = receive {mosaic_process_migrator, rollback, SourceToken, succeeded} -> ok end,
					ok = stop_and_wait_process (Source),
					ok;
				{rollback, fail} ->
					{error, failed} = mosaic_process:rollback_migration (Source, SourceToken),
					ok = receive {mosaic_process_migrator, rollback, SourceToken, failed, failed} -> ok end,
					ok = wait_process (Source, [{migration_failed, failed}]),
					ok
			end
	end,
	ok.


test_migrate_as_target (Completion) ->
	Self = erlang:self (),
	TargetToken = erlang:make_ref (),
	{ok, Target} = start_link_process (mosaic_process_tester, {migrate, TargetToken}),
	ok = case Completion of
		{reject} ->
			{error, rejected} = mosaic_process:begin_migration (Target, TargetToken, reject, Self),
			ok = wait_process (Target, [{migration_failed, rejected}]),
			ok;
		{terminate} ->
			{error, terminated} = mosaic_process:begin_migration (Target, TargetToken, terminate, Self),
			ok = wait_process (Target, [{migration_failed, terminated}]),
			ok;
		{_, CompletionOutcome} ->
			ok = mosaic_process:begin_migration (Target, TargetToken, {continue, CompletionOutcome}, Self),
			ok = receive {mosaic_process_migrator, 'begin', TargetToken, succeeded} -> ok end,
			ok = receive {mosaic_process_migrator, continue, TargetToken, completed} -> ok end,
			ok = case Completion of
				{commit, succeed} ->
					ok = mosaic_process:commit_migration (Target, TargetToken),
					ok = receive {mosaic_process_migrator, commit, TargetToken, succeeded} -> ok end,
					ok = stop_and_wait_process (Target),
					ok;
				{commit, fail} ->
					{error, failed} = mosaic_process:commit_migration (Target, TargetToken),
					ok = receive {mosaic_process_migrator, commit, TargetToken, failed, failed} -> ok end,
					ok = wait_process (Target, [{migration_failed, failed}]),
					ok;
				{rollback, succeed} ->
					ok = mosaic_process:rollback_migration (Target, TargetToken),
					ok = receive {mosaic_process_migrator, rollback, TargetToken, succeeded} -> ok end,
					ok = wait_process (Target),
					ok;
				{rollback, fail} ->
					{error, failed} = mosaic_process:rollback_migration (Target, TargetToken),
					ok = receive {mosaic_process_migrator, rollback, TargetToken, failed, failed} -> ok end,
					ok = wait_process (Target, [{migration_failed, failed}]),
					ok
			end
	end,
	ok.


start_link_process (Module, Disposition) ->
	start_link_process (Module, Disposition, defaults).

start_link_process (Module, Disposition, Configuration) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	start_link_process (Module, Disposition, Identifier, Configuration).

start_link_process (Module, Disposition, Identifier, Configuration) ->
	start_link_process (noname, Module, Disposition, Identifier, Configuration).

start_link_process (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_tests:enforce_start_outcome (QualifiedName,
			mosaic_process:start_link (QualifiedName, Module, Disposition, Identifier, Configuration)).

stop_process (Process) ->
	stop_process (Process, normal).

stop_process (Process, Signal) ->
	mosaic_tests:enforce_stop_outcome (
			mosaic_process:stop (Process, Signal)).

stop_and_wait_process (Process) ->
	stop_and_wait_process (Process, normal, [normal]).

stop_and_wait_process (Process, Signal, Reasons) ->
	mosaic_tests:enforce_stop_outcome_and_wait (Process, Reasons, mosaic_process:stop (Process, Signal)).

wait_process (Process) ->
	wait_process (Process, [normal]).

wait_process (Process, Reasons) ->
	mosaic_tests:enforce_stop_outcome_and_wait (Process, Reasons, ok).

call_process (Process, Request) ->
	call_process (Process, Request, <<>>).

call_process (Process, Request, RequestData) ->
	mosaic_process:call (Process, Request, RequestData).

cast_process (Process, Request) ->
	cast_process (Process, Request, <<>>).

cast_process (Process, Request, RequestData) ->
	mosaic_process:cast (Process, Request, RequestData).


test () ->
	mosaic_tests:test_module (mosaic_process_tests).


configure (dummy, create, Identifier, term, defaults, defaults) ->
	Configuration = {
			{spawn_executable, <<"./.outputs/gcc/applications-elf/mosaic_port_process_dummy.elf">>},
			[{arg0, <<"[mosaic_process#dummy#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
	{ok, mosaic_port_process, Configuration};
	
configure (dummy, {migrate, source}, _Identifier, term, defaults, defaults) ->
	{ok, none, defaults};
	
configure (dummy, {migrate, target}, _Identifier, term, defaults, defaults) ->
	{ok, mosaic_port_process, defaults};
	
configure (dummy, _Disposition, _Identifier, term, Configuration, defaults) ->
	{error, {invalid_configuration, Configuration}};
	
configure (dummy, Disposition, Identifier, json, null, defaults) ->
	configure (dummy, Disposition, Identifier, term, defaults, defaults);
	
configure (dummy, Disposition, Identifier, json, {struct, []}, defaults) ->
	configure (dummy, Disposition, Identifier, term, defaults, defaults);
	
configure (dummy, _Disposition, _Identifier, json, Configuration, defaults) ->
	{error, {invalid_configuration, Configuration}}.
