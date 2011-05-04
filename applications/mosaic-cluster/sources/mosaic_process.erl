
-module (mosaic_process).

-export ([behaviour_info/1]).
-export ([start/2, start/3, start_link/3, start_link/2]).
-export ([start_supervised/3]).
-export ([stop/1, stop/2]).
-export ([call/2, call/3, cast/2]).
-export ([begin_migration/4, commit_migration/2, rollback_migration/2]).


behaviour_info (callbacks) ->
	[
		{init, 1},
		{terminate, 2},
		{handle_stop, 2},
		{handle_call, 3},
		{handle_cast, 2},
		{handle_info, 2},
		{begin_migration, 2},
		{commit_migration, 1},
		{rollback_migration, 1}].


start (Module, Disposition) ->
	start (noname, Module, Disposition).

start (QualifiedName, Module, Disposition) ->
	mosaic_tools:start (gen_server, mosaic_process_internals, QualifiedName, {Module, Disposition}).


start_link (Module, Disposition) ->
	start_link (noname, Module, Disposition).

start_link (QualifiedName, Module, Disposition) ->
	mosaic_tools:start_link (gen_server, mosaic_process_internals, QualifiedName, {Module, Disposition}).


start_supervised (QualifiedName, Module, Disposition) ->
	mosaic_sup:start_child_process (QualifiedName, Module, Disposition).


stop (Process) ->
	stop (Process, normal).

stop (Process, Signal)
		when (is_pid (Process) orelse is_atom (Process)) ->
	gen_server:call (Process, {stop, Signal}).


call (Process, Request) ->
	call (Process, Request, 5000).

call (Process, Request, Timeout)
		when (is_pid (Process) orelse is_atom (Process)),
			((Timeout =:= infinity) orelse (is_number (Timeout) andalso (Timeout > 0))) ->
	gen_server:call (Process, {call, Request}, Timeout).

cast (Process, Request)
		when (is_pid (Process) orelse is_atom (Process)) ->
	gen_server:cast (Process, {cast, Request}).


begin_migration (Process, Token, Arguments, Monitor)
		when is_pid (Process), is_pid (Monitor) ->
	gen_server:call (Process, {begin_migration, Token, Arguments, Monitor}, infinity).

commit_migration (Process, Token)
		when is_pid (Process) ->
	gen_server:call (Process, {commit_migration, Token}, infinity).

rollback_migration (Process, Token)
		when is_pid (Process) ->
	gen_server:call (Process, {rollback_migration, Token}, infinity).
