
-module (mosaic_process).


-export ([behaviour_info/1]).
-export ([start/4, start/5, start_link/4, start_link/5]).
-export ([start_supervised/5]).
-export ([stop/1, stop/2]).
-export ([call/3, call/4, call/5, cast/3, cast/4]).
-export ([begin_migration/4, commit_migration/2, rollback_migration/2]).


behaviour_info (callbacks) ->
	[
		{init, 3},
		{terminate, 2},
		{handle_stop, 2},
		{handle_call, 5},
		{handle_cast, 4},
		{handle_info, 2},
		{begin_migration, 4},
		{commit_migration, 1},
		{rollback_migration, 1}].


start (Module, Disposition, Identifier, Configuration) ->
	start (noname, Module, Disposition, Identifier, Configuration).

start (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_process_tools:start (gen_server, mosaic_process_internals, QualifiedName, {Module, Disposition, Identifier, Configuration}).


start_link (Module, Disposition, Identifier, Configuration) ->
	start_link (noname, Module, Disposition, Identifier, Configuration).

start_link (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_process_internals, QualifiedName, {Module, Disposition, Identifier, Configuration}).


start_supervised (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_cluster_sup:start_child_process (QualifiedName, Module, Disposition, Identifier, Configuration).


stop (Process) ->
	stop (Process, normal).

stop (Process, Signal)
		when (is_pid (Process) orelse is_atom (Process)) ->
	try gen_server:call (Process, {mosaic_process, stop, Signal}) of
		ok -> ok;
		Outcome = {ok, _Reply} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


call (Process, Operation, Inputs) ->
	call (Process, Operation, Inputs, <<>>).

call (Process, Operation, Inputs, Data) ->
	call (Process, Operation, Inputs, Data, default).

call (Process, Operation, Inputs, Data, Timeout)
		when (is_pid (Process) orelse is_atom (Process)), is_binary (Operation), is_binary (Data),
			((Timeout =:= default) orelse (Timeout =:= infinity) orelse (is_integer (Timeout) andalso (Timeout > 0))) ->
	try gen_server:call (Process, {mosaic_process, call, Operation, Inputs, Data}, if (Timeout =:= default) -> 5000; true -> Timeout end) of
		Outcome = {ok, _Outputs, Data} when is_binary (Data) -> Outcome;
		Error = {error, _Reason, Data} when is_binary (Data) -> Error;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


cast (Process, Operation, Inputs) ->
	cast (Process, Operation, Inputs, <<>>).

cast (Process, Operation, Inputs, Data)
		when (is_pid (Process) orelse is_atom (Process)), is_binary (Operation), is_binary (Data) ->
	ok = gen_server:cast (Process, {mosaic_process, cast, Operation, Inputs, Data}),
	ok.


begin_migration (Process, Token, Configuration, Monitor)
		when is_pid (Process), is_pid (Monitor) ->
	try gen_server:call (Process, {mosaic_process, begin_migration, Token, Configuration, Monitor}, infinity) of
		Outcome = ok -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


commit_migration (Process, Token)
		when is_pid (Process) ->
	try gen_server:call (Process, {mosaic_process, commit_migration, Token}, infinity) of
		Outcome = ok -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


rollback_migration (Process, Token)
		when is_pid (Process) ->
	try gen_server:call (Process, {mosaic_process, rollback_migration, Token}, infinity) of
		Outcome = ok -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.
