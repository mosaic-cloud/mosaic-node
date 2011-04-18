
-module (mosaic_process_tests).

-export ([test/0]).


%-define (process_tester__enabled, _).
%-define (process_controller__enabled, _).
%-define (process_migrator__enabled, _).
%-define (port_process__enabled, _).
-define (dummy_process__enabled, _).


-export ([
		process_tester__start_stop/1,
		process_tester__ping/1,
		process_tester__start_error/1,
		process_tester__stop_error/1,
		process_tester__migrate_as_source/1,
		process_tester__migrate_as_target/1]).

-ifdef (process_tester__enabled).
-test ({process_tester__start_stop, [{stop_method, stop}, {stop_reason, normal}]}).
-test ({process_tester__start_stop, [{stop_method, stop}, {stop_reason, reference}]}).
-test ({process_tester__start_stop, [{stop_method, call}, {stop_reason, normal}]}).
-test ({process_tester__start_stop, [{stop_method, cast}, {stop_reason, normal}]}).
-test ({process_tester__start_stop, [{stop_method, info}, {stop_reason, normal}]}).
-test ({process_tester__start_error, defaults}).
-test ({process_tester__stop_error, defaults}).
-test ({process_tester__ping, [{ping_method, call_reply}]}).
-test ({process_tester__ping, [{ping_method, call_noreply}]}).
-test ({process_tester__ping, [{ping_method, cast}]}).
-test ({process_tester__ping, [{ping_method, info}]}).
-test ({process_tester__migrate_as_source, reject}).
-test ({process_tester__migrate_as_source, terminate}).
-test ({process_tester__migrate_as_source, {commit, succeed}}).
-test ({process_tester__migrate_as_source, {commit, fail}}).
-test ({process_tester__migrate_as_source, {rollback, succeed}}).
-test ({process_tester__migrate_as_source, {rollback, fail}}).
-test ({process_tester__migrate_as_target, reject}).
-test ({process_tester__migrate_as_target, terminate}).
-test ({process_tester__migrate_as_target, {commit, succeed}}).
-test ({process_tester__migrate_as_target, {commit, fail}}).
-test ({process_tester__migrate_as_target, {rollback, succeed}}).
-test ({process_tester__migrate_as_target, {rollback, fail}}).
-endif.


-export ([
		process_controller__start_stop/1,
		process_controller__start_error/1,
		process_controller__create/1,
		process_controller__migrate/1]).

-ifdef (process_controller__enabled).
%-test ({process_controller__start_stop, defaults}).
%-test ({process_controller__start_error, defaults}).
%-test ({process_controller__create, succeed}).
%-test ({process_controller__create, {fail, module}}).
%-test ({process_controller__create, {fail, failed}}).
-test ({process_controller__migrate, defaults}).
-endif.


-export ([
		process_migrator__migrate/1]).

-ifdef (process_migrator__enabled).
-test ({process_migrator__migrate, defaults}).
-endif.


-export ([
		port_process__start_stop/1,
		port_process__migrate_as_source/1,
		port_process__migrate_as_target/1]).

-ifdef (port_process__enabled).
-test ({port_process__start_stop, defaults}).
-test ({port_process__migrate_as_source, commit}).
-test ({port_process__migrate_as_source, rollback}).
-test ({port_process__migrate_as_target, commit}).
-test ({port_process__migrate_as_target, rollback}).
-endif.


-export ([
		dummy_process__start_stop/1,
		dummy_process__migrate/1]).

-ifdef (dummy_process__enabled).
-test ({dummy_process__start_stop, defaults}).
-test ({dummy_process__migrate, defaults}).
-endif.


process_tester__start_stop ([{stop_method, StopMethod}, {stop_reason, reference}]) ->
	process_tester__start_stop ([{stop_method, StopMethod}, {stop_reason, erlang:make_ref ()}]);
	
process_tester__start_stop ([{stop_method, StopMethod}, {stop_reason, StopReason}]) ->
	Name = mosaic_process_test,
	Token = erlang:make_ref (),
	{ok, Process} = start_process (Name, mosaic_process_tester, {create, ok}),
	ok = case StopMethod of
		stop ->
			{ok, Token} = mosaic_process:stop (Process, {stop, StopReason, {ok, Token}}, 1000),
			ok;
		call ->
			{ok, Token} = mosaic_process:call (Process, {stop, StopReason, {ok, Token}}, 1000),
			ok;
		cast ->
			ok = mosaic_process:cast (Process, {stop, StopReason}),
			ok;
		info ->
			Process ! {stop, StopReason},
			ok
	end,
	{ok, StopReason} = join_process (Process, [StopReason]),
	ok.

process_tester__start_error (defaults) ->
	Name = mosaic_process_test,
	ErrorReason = erlang:make_ref (),
	{error, ErrorReason} = start_process (Name, mosaic_process_tester, {create, {stop, ErrorReason}}),
	undefined = erlang:whereis (mosaic_process_test),
	ok.

process_tester__stop_error (defaults) ->
	Name = mosaic_process_test,
	ErrorReason = erlang:make_ref (),
	{ok, Process} = start_process (Name, mosaic_process_tester, {create, ok}),
	{error, ErrorReason} = mosaic_process:stop (Process, {reply, {error, ErrorReason}}, 1000),
	ok = stop_process (Process, normal),
	ok.

process_tester__ping ([{ping_method, PingMethod}]) ->
	Name = mosaic_process_test,
	Self = erlang:self (),
	Token1 = erlang:make_ref (),
	{ok, Process} = start_process (Name, mosaic_process_tester, {create, ok}),
	{ok, Token2} = case PingMethod of
		call_reply ->
			{pong, Token1, Token2_} = mosaic_process:call (Process, {ping, Self, Token1, reply}, 1000),
			{ok, Token2_};
		call_noreply ->
			{pong, Token1, Token2_} = mosaic_process:call (Process, {ping, Self, Token1, noreply}, 1000),
			{ok, Token2_};
		cast ->
			ok = mosaic_process:cast (Process, {ping, Self, Token1}),
			{ok, Token1};
		info ->
			Process ! {ping, Self, Token1},
			{ok, Token1}
	end,
	ok = receive {pong, Token1, Token2} -> ok after 1000 -> receive_timeout () end,
	ok = stop_process (Process, normal),
	ok.

process_tester__migrate_as_source (Completion) ->
	SourceName = mosaic_process_test_source,
	Self = erlang:self (),
	Token = erlang:make_ref (),
	Arguments = erlang:make_ref (),
	{ok, Source} = start_process (SourceName, mosaic_process_tester, {create, ok}),
	ok = case Completion of
		reject ->
			{error, rejected} = mosaic_process:begin_migration (Source, Token, reject, Self),
			ok = stop_process (Source, normal),
			ok;
		terminate ->
			{error, terminated} = mosaic_process:begin_migration (Source, Token, terminate, Self),
			{ok, {migration_failed, terminated}} = join_process (Source, [{migration_failed, terminated}]),
			ok;
		{_, CompletionOutcome} ->
			ok = mosaic_process:begin_migration (Source, Token, {continue, Arguments, CompletionOutcome}, Self),
			ok = receive {begin_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
			ok = receive {continue_migration, Token, prepared, Arguments} -> ok after 1000 -> receive_timeout () end,
			ok = receive {continue_migration, Token, completed} -> ok after 1000 -> receive_timeout () end,
			case Completion of
				{commit, succeed} ->
					ok = mosaic_process:commit_migration (Source, Token),
					ok = receive {commit_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
					{ok, normal} = join_process (Source, [normal]),
					ok;
				{commit, fail} ->
					{error, failed} = mosaic_process:commit_migration (Source, Token),
					ok = receive {commit_migration, Token, failed, failed} -> ok after 1000 -> receive_timeout () end,
					{ok, {migration_failed, failed}} = join_process (Source, [{migration_failed, failed}]),
					ok;
				{rollback, succeed} ->
					ok = mosaic_process:rollback_migration (Source, Token),
					ok = receive {rollback_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
					ok = stop_process (Source, normal),
					ok;
				{rollback, fail} ->
					{error, failed} = mosaic_process:rollback_migration (Source, Token),
					ok = receive {rollback_migration, Token, failed, failed} -> ok after 1000 -> receive_timeout () end,
					{ok, {migration_failed, failed}} = join_process (Source, [{migration_failed, failed}]),
					ok
			end
	end,
	ok.

process_tester__migrate_as_target (Completion) ->
	TargetName = mosaic_process_test_target,
	Self = erlang:self (),
	Token = erlang:make_ref (),
	{ok, Target} = start_process (TargetName, mosaic_process_tester, {migrate, Token}),
	ok = case Completion of
		reject ->
			{error, rejected} = mosaic_process:begin_migration (Target, Token, reject, Self),
			{ok, {migration_failed, rejected}} = join_process (Target, [{migration_failed, rejected}]),
			ok;
		terminate ->
			{error, terminated} = mosaic_process:begin_migration (Target, Token, terminate, Self),
			{ok, {migration_failed, terminated}} = join_process (Target, [{migration_failed, terminated}]),
			ok;
		{_, CompletionOutcome} ->
			ok = mosaic_process:begin_migration (Target, Token, {continue, CompletionOutcome}, Self),
			ok = receive {begin_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
			ok = receive {continue_migration, Token, completed} -> ok after 1000 -> receive_timeout () end,
			ok = case Completion of
				{commit, succeed} ->
					ok = mosaic_process:commit_migration (Target, Token),
					ok = receive {commit_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
					ok = stop_process (Target, normal),
					ok;
				{commit, fail} ->
					{error, failed} = mosaic_process:commit_migration (Target, Token),
					ok = receive {commit_migration, Token, failed, failed} -> ok after 1000 -> receive_timeout () end,
					{ok, {migration_failed, failed}} = join_process (Target, [{migration_failed, failed}]),
					ok;
				{rollback, succeed} ->
					ok = mosaic_process:rollback_migration (Target, Token),
					ok = receive {rollback_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
					{ok, normal} = join_process (Target, [normal]),
					ok;
				{rollback, fail} ->
					{error, failed} = mosaic_process:rollback_migration (Target, Token),
					ok = receive {rollback_migration, Token, failed, failed} -> ok after 1000 -> receive_timeout () end,
					{ok, {migration_failed, failed}} = join_process (Target, [{migration_failed, failed}]),
					ok
			end
	end,
	ok.


process_controller__start_stop (defaults) ->
	Name = mosaic_process_test,
	{ok, ProcessSupervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	true = erlang:unlink (ProcessSupervisor),
	{ok, Controller} = mosaic_process_controller:start_link (Name, defaults),
	ok = mosaic_process_controller:stop (Controller),
	{ok, normal} = join_process (Controller, [normal]),
	true = erlang:exit (ProcessSupervisor, normal),
	ok.

process_controller__start_error (defaults) ->
	Name = mosaic_process_test,
	{error, process_supervisor_does_not_exist} = mosaic_process_controller:start_link (Name, defaults),
	ok.

process_controller__create (Outcome) ->
	Name = mosaic_process_test,
	{ok, ProcessSupervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, Controller} = mosaic_process_controller:start_link (Name, defaults),
	case Outcome of
		succeed ->
			{ok, Process} = mosaic_process_controller:create (Controller, key, mosaic_process_tester, ok),
			true = erlang:link (Process),
			ok = stop_process (Process, normal);
		{fail, failed} ->
			{error, failed} = mosaic_process_controller:create (Controller, key, mosaic_process_tester, {stop, failed});
		{fail, module} ->
			{error, {undef, [{undefined_module, init, _} | _]}} = mosaic_process_controller:create (Controller, key, undefined_module, undefined)
	end,
	ok = mosaic_process_controller:stop (Controller),
	{ok, normal} = join_process (Controller, [normal]),
	true = erlang:exit (ProcessSupervisor, normal),
	ok.

process_controller__migrate (defaults) ->
	SourceName = mosaic_process_test_source,
	TargetName = mosaic_process_test_target,
	Self = erlang:self (),
	Token = erlang:make_ref (),
	{ok, ProcessSupervisor} = mosaic_cluster_sup:start_link (mosaic_process_sup),
	{ok, Source} = mosaic_process_controller:start_link (SourceName, defaults),
	{ok, Target} = mosaic_process_controller:start_link (TargetName, defaults),
	{ok, SourceProcess} = mosaic_process_controller:create (Source, key, mosaic_process_tester, ok),
	true = erlang:link (SourceProcess),
	{ok, Migrator, TargetProcess} = mosaic_process_controller:migrate (Source, Target, key, {continue, {continue, succeed}, succeed}, Self, Token),
	true = erlang:link (Migrator),
	true = erlang:link (TargetProcess),
	{ok, normal} = join_process (Migrator, [normal]),
	{ok, normal} = join_process (SourceProcess, [normal]),
	ok = mosaic_process_controller:stop (Target, key, normal),
	{ok, normal} = join_process (TargetProcess, [normal]),
	ok = mosaic_process_controller:stop (Source),
	ok = mosaic_process_controller:stop (Target),
	{ok, normal} = join_process (Source, [normal]),
	{ok, normal} = join_process (Target, [normal]),
	true = erlang:exit (ProcessSupervisor, normal),
	ok.


process_migrator__migrate (defaults) ->
	SourceName = mosaic_process_test_source,
	SourceToken = erlang:make_ref (),
	TargetName = mosaic_process_test_target,
	TargetToken = erlang:make_ref (),
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	TesterArguments = {continue, {continue, succeed}, succeed},
	{ok, Source} = start_process (SourceName, mosaic_process_tester, {create, ok}),
	{ok, Target} = start_process (TargetName, mosaic_process_tester, {migrate, TargetToken}),
	{ok, Migrator} = mosaic_process_migrator:start (Source, SourceToken, Target, TargetToken, Self, SelfToken),
	true = erlang:link (Migrator),
	ok = mosaic_process_migrator:migrate (Migrator, TesterArguments),
	{ok, normal} = join_process (Source, [normal]),
	{ok, normal} = join_process (Migrator, [normal]),
	ok = stop_process (Target, normal),
	ok.


port_process__start_stop (defaults) ->
	Name = mosaic_process_test,
	Arguments = {{spawn_executable, "/bin/cat"}, []},
	{ok, Process} = start_process (Name, mosaic_port_process, {create, Arguments}),
	ok = stop_process (Process, normal),
	ok.

port_process__migrate_as_source (CompletionMethod) ->
	SourceName = mosaic_process_test_source,
	Arguments = {{spawn_executable, "/bin/cat"}, []},
	Token = erlang:make_ref (),
	{ok, Source} = start_process (SourceName, mosaic_port_process, {create, Arguments}),
	ok = mosaic_process:begin_migration (Source, Token, none, erlang:self ()),
	ok = receive {begin_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
	ok = receive {continue_migration, Token, prepared, Arguments} -> ok after 1000 -> receive_timeout () end,
	ok = receive {continue_migration, Token, completed} -> ok after 1000 -> receive_timeout () end,
	ok = case CompletionMethod of
		commit ->
			ok = mosaic_process:commit_migration (Source, Token),
			ok = receive {commit_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
			{ok, normal} = join_process (Source, [normal]),
			ok;
		rollback ->
			ok = mosaic_process:rollback_migration (Source, Token),
			ok = receive {rollback_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
			ok = stop_process (Source, normal),
			ok
	end,
	ok.

port_process__migrate_as_target (CompletionMethod) ->
	TargetName = mosaic_process_test_target,
	Arguments = {{spawn_executable, "/bin/cat"}, []},
	Token = erlang:make_ref (),
	{ok, Target} = start_process (TargetName, mosaic_port_process, {migrate, Token}),
	ok = mosaic_process:begin_migration (Target, Token, Arguments, erlang:self ()),
	ok = receive {begin_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
	ok = receive {continue_migration, Token, completed} -> ok after 1000 -> receive_timeout () end,
	ok = case CompletionMethod of
		commit ->
			ok = mosaic_process:commit_migration (Target, Token),
			ok = receive {commit_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
			ok = stop_process (Target, normal),
			ok;
		rollback ->
			ok = mosaic_process:rollback_migration (Target, Token),
			ok = receive {rollback_migration, Token, succeeded} -> ok after 1000 -> receive_timeout () end,
			{ok, normal} = join_process (Target, [normal]),
			ok
	end,
	ok.


dummy_process__start_stop (defaults) ->
	Name = mosaic_process_test,
	{ok, Process} = start_process (Name, mosaic_dummy_process, {create, defaults}),
	ok = stop_process (Process, normal),
	ok.

dummy_process__migrate (defaults) ->
	SourceName = mosaic_process_test_source,
	SourceToken = erlang:make_ref (),
	TargetName = mosaic_process_test_target,
	TargetToken = erlang:make_ref (),
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	{ok, Source} = start_process (SourceName, mosaic_dummy_process, {create, defaults}),
	{ok, Target} = start_process (TargetName, mosaic_dummy_process, {migrate, TargetToken}),
	{ok, Migrator} = mosaic_process_migrator:start (Source, SourceToken, Target, TargetToken, Self, SelfToken),
	true = erlang:link (Migrator),
	ok = mosaic_process_migrator:migrate (Migrator, none),
	{ok, normal} = join_process (Source, [normal]),
	{ok, normal} = join_process (Migrator, [normal]),
	ok = stop_process (Target, normal),
	ok.


start_process (Name, Module, Arguments)
		when is_atom (Name), is_atom (Module) ->
	case mosaic_process:start_link (Name, Module, Arguments) of
		{ok, Process} when is_pid (Process) ->
			case erlang:whereis (Name) of
				Process ->
					true = erlang:link (Process),
					{ok, Process};
				OtherProcess when (Process =/= OtherProcess) ->
					{mismatched_process_name, Name, Process, OtherProcess};
				undefined ->
					{mismatched_process_name, Name, Process}
			end;
		Error = {error, _Reason} ->
			Error
	end.

join_process (Process, Reasons)
		when is_pid (Process), is_list (Reasons) ->
	receive
		{'EXIT', Process, Reason} ->
			case lists:member (Reason, Reasons) of
				true ->
					{ok, Reason};
				false ->
					{unexpected_reason, Reason}
			end
	after 1000 ->
		timeout
	end.

stop_process (Process, Reason) ->
	ok = mosaic_process:stop (Process, Reason, 1000),
	{ok, normal} = join_process (Process, [Reason]),
	ok.

trace_process (Process)
		when is_pid (Process) ->
	Loop = fun (Loop) ->
		receive
			Trace ->
				ok = error_logger:info_report ([lists:flatten (io_lib:format ("Traced...~n~p", [Trace]))]),
				Loop (Loop)
		end
	end,
	Tracer = spawn_link (fun () -> Loop (Loop) end),
	1 = erlang:trace (Process, true, [send, 'receive', procs, {tracer, Tracer}]),
	ok.

receive_timeout () ->
	receive
		Message ->
			{unexpected_message, Message}
	after 0 ->
		timeout
	end.


test () ->
	OldTrapExit = erlang:process_flag (trap_exit, true),
	Specifications = lists:flatten (
			lists:map (
				fun ({test, Specification}) -> [Specification]; ({_, _}) -> [] end,
				mosaic_process_tests:module_info (attributes))),
	Outcome = test (Specifications),
	true = erlang:process_flag (trap_exit, OldTrapExit),
	ok = case Outcome of
		ok ->
			ok = error_logger:info_report (["Testing succeeded!"]),
			ok;
		{failed, _Test, Reason} ->
			ok = error_logger:error_report ([lists:flatten (io_lib:format ("Testing failed!~n~p", [Reason]))]),
			ok;
		{unexpected_message, Message} ->
			ok = error_logger:error_report ([lists:flatten (io_lib:format ("Testing failed!~n~p", [{unexpected_message, Message}]))]),
			ok
	end,
	ok.

test ([Test = {TestFunction, TestArguments} | Tests])
		when is_atom (TestFunction) ->
	ok = error_logger:info_report ([lists:flatten (io_lib:format ("Testing `~w` with `~w`...", [TestFunction, TestArguments]))]),
	Token = erlang:make_ref (),
	Slave = erlang:spawn_link (
			fun () ->
				false = erlang:process_flag (trap_exit, true),
				ok = erlang:apply (mosaic_process_tests, TestFunction, [TestArguments]),
				exit ({succeeded, Token})
			end),
	receive
		{'EXIT', Slave, {succeeded, Token}} ->
			test (Tests);
		{'EXIT', Slave, Reason} ->
			{failed, Test, Reason};
		Message ->
			{unexpected_message, Message}
	after 10000 ->
		{failed, Test, timeout}
	end;
	
test ([]) ->
	ok.
