
-module (mosaic_process).

-export ([behaviour_info/1]).
-export ([start/2, start/3, start_link/3, start_link/2, start_supervised/2, start_supervised/4]).
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

start (QualifiedName = {local, LocalName}, Module, Disposition)
		when is_atom (LocalName), is_atom (Module), (is_record (Disposition, create, 2) or is_record (Disposition, migrate, 2)) ->
	gen_server:start (QualifiedName, mosaic_process_internals, {QualifiedName, Module, Disposition}, []);
	
start (QualifiedName = noname, Module, Disposition)
		when is_atom (Module), (is_record (Disposition, create, 2) or is_record (Disposition, migrate, 2)) ->
	gen_server:start (mosaic_process_internals, {QualifiedName, Module, Disposition}, []).


start_link (Module, Disposition) ->
	start_link (noname, Module, Disposition).

start_link (QualifiedName = {local, LocalName}, Module, Disposition)
		when is_atom (LocalName), is_atom (Module), (is_record (Disposition, create, 2) or is_record (Disposition, migrate, 2)) ->
	gen_server:start_link (QualifiedName, mosaic_process_internals, {QualifiedName, Module, Disposition}, []);
	
start_link (QualifiedName = noname, Module, Disposition)
		when is_atom (Module), (is_record (Disposition, create, 2) or is_record (Disposition, migrate, 2)) ->
	gen_server:start_link (mosaic_process_internals, {QualifiedName, Module, Disposition}, []).


start_supervised (Module, Disposition) ->
	start_supervised (noname, Module, Disposition).

start_supervised (QualifiedName, Module, Disposition) ->
	start_supervised (mosaic_process_sup, QualifiedName, Module, Disposition).

start_supervised (Supervisor, QualifiedName, Module, Disposition)
		when is_atom (Module), (is_record (Disposition, create, 2) or is_record (Disposition, migrate, 2)) ->
	mosaic_cluster_sup:start_child_process (Supervisor, QualifiedName, Module, Disposition).


stop (Process) ->
	stop (Process, normal).

stop (Process, Signal)
		when (is_pid (Process) or is_atom (Process)) ->
	gen_server:call (Process, {stop, Signal}).


call (Process, Request) ->
	call (Process, Request, 5000).

call (Process, Request, Timeout)
		when (is_pid (Process) or is_atom (Process)), ((Timeout =:= infinity) or (is_number (Timeout) and (Timeout > 0))) ->
	gen_server:call (Process, {call, Request}, Timeout).

cast (Process, Request)
		when (is_pid (Process) or is_atom (Process)) ->
	gen_server:cast (Process, {cast, Request}).


begin_migration (Process, Token, Arguments, Monitor)
		when (is_pid (Process) or is_atom (Process)), is_pid (Monitor) ->
	gen_server:call (Process, {begin_migration, Token, Arguments, Monitor}, infinity).

commit_migration (Process, Token)
		when (is_pid (Process) or is_atom (Process)) ->
	gen_server:call (Process, {commit_migration, Token}, infinity).

rollback_migration (Process, Token)
		when (is_pid (Process) or is_atom (Process)) ->
	gen_server:call (Process, {rollback_migration, Token}, infinity).
