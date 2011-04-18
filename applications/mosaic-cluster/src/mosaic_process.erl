
-module (mosaic_process).

-export ([behaviour_info/1]).
-export ([start/3, start_link/3, start_supervised/4]).
-export ([stop/1, stop/2, stop/3]).
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

start (Name, Module, Disposition)
		when is_atom (Name), is_atom (Module), is_tuple (Disposition) ->
	gen_server:start ({local, Name}, mosaic_process_internals, {Name, Module, Disposition}, []).

start_link (Name, Module, Disposition)
		when is_atom (Name), is_atom (Module), is_tuple (Disposition) ->
	gen_server:start_link ({local, Name}, mosaic_process_internals, {Name, Module, Disposition}, []).

start_supervised (Supervisor, Name, Module, Disposition)
		when (is_pid (Supervisor) or is_atom (Supervisor)), is_atom (Name), is_atom (Module), is_tuple (Disposition) ->
	mosaic_cluster_sup:start_child_process (Supervisor, Name, Module, Disposition).

stop (Process) ->
	stop (Process, normal).

stop (Process, Signal) ->
	stop (Process, Signal, 5000).

stop (Process, Signal, Timeout)
		when is_pid (Process) or is_atom (Process),
				(Timeout =:= infinity) or (is_number (Timeout) and (Timeout > 0)) ->
	gen_server:call (Process, {stop, Signal}, Timeout).

call (Process, Request) ->
	call (Process, Request, 5000).

call (Process, Request, Timeout)
		when is_pid (Process) or is_atom (Process), (Timeout =:= infinity) or (is_number (Timeout) and (Timeout > 0)) ->
	gen_server:call (Process, {call, Request}, Timeout).

cast (Process, Request)
		when is_pid (Process) or is_atom (Process) ->
	gen_server:cast (Process, {cast, Request}).

begin_migration (Process, Token, Arguments, Monitor)
		when is_pid (Process) or is_atom (Process), is_pid (Monitor) ->
	gen_server:call (Process, {begin_migration, Token, Arguments, Monitor}, infinity).

commit_migration (Process, Token)
		when is_pid (Process) or is_atom (Process) ->
	gen_server:call (Process, {commit_migration, Token}, infinity).

rollback_migration (Process, Token)
		when is_pid (Process) or is_atom (Process) ->
	gen_server:call (Process, {rollback_migration, Token}, infinity).
