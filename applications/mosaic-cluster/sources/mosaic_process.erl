
-module (mosaic_process).


-export ([behaviour_info/1]).
-export ([start/4, start/5, start_link/4, start_link/5]).
-export ([start_supervised/5]).
-export ([stop/1, stop/2]).
-export ([call/2, call/3, call/4, cast/2, cast/3]).
-export ([begin_migration/4, commit_migration/2, rollback_migration/2]).


behaviour_info (callbacks) ->
	[
		{init, 3},
		{terminate, 2},
		{handle_stop, 2},
		{handle_call, 4},
		{handle_cast, 3},
		{handle_info, 2},
		{begin_migration, 4},
		{commit_migration, 1},
		{rollback_migration, 1}].


start (Module, Disposition, Identifier, Configuration) ->
	start (noname, Module, Disposition, Identifier, Configuration).

start (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_tools:start (gen_server, mosaic_process_internals, QualifiedName, {Module, Disposition, Identifier, Configuration}).


start_link (Module, Disposition, Identifier, Configuration) ->
	start_link (noname, Module, Disposition, Identifier, Configuration).

start_link (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_tools:start_link (gen_server, mosaic_process_internals, QualifiedName, {Module, Disposition, Identifier, Configuration}).


start_supervised (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	mosaic_sup:start_child_process (QualifiedName, Module, Disposition, Identifier, Configuration).


stop (Process) ->
	stop (Process, normal).

stop (Process, Signal)
		when (is_pid (Process) orelse is_atom (Process)) ->
	gen_server:call (Process, {mosaic_process, stop, Signal}).


call (Process, Request) ->
	call (Process, Request, <<>>).

call (Process, Request, RequestData) ->
	call (Process, Request, RequestData, default).

call (Process, Request, RequestData, Timeout)
		when (is_pid (Process) orelse is_atom (Process)), is_binary (RequestData),
			((Timeout =:= default) orelse (Timeout =:= infinity) orelse (is_number (Timeout) andalso (Timeout > 0))) ->
	try
		case gen_server:call (Process, {mosaic_process, call, Request, RequestData}, case Timeout of default -> 5000; _ -> Timeout end) of
			Outcome = {ok, _Reply, ReplyData} when is_binary (ReplyData) ->
				Outcome;
			Error = {error, _Reason} ->
				Error;
			Outcome ->
				{error, {invalid_outcome, Outcome}}
		end
	catch
		error : Reason ->
			{error, Reason};
		throw : Reason ->
			{error, Reason};
		exit : Reason ->
			{error, Reason}
	end.


cast (Process, Request) ->
	cast (Process, Request, <<>>).

cast (Process, Request, RequestData)
		when (is_pid (Process) orelse is_atom (Process)), is_binary (RequestData) ->
	try
		ok = gen_server:cast (Process, {mosaic_process, cast, Request, RequestData})
	catch
		throw : Reason ->
			{error, Reason};
		error : Reason ->
			{error, Reason};
		exit : Reason ->
			{error, Reason}
	end.


begin_migration (Process, Token, Configuration, Monitor)
		when is_pid (Process), is_pid (Monitor) ->
	try
		case gen_server:call (Process, {mosaic_process, begin_migration, Token, Configuration, Monitor}, infinity) of
			Outcome = ok ->
				Outcome;
			Error = {error, _Reason} ->
				Error;
			Outcome ->
				{error, {invalid_outcome, Outcome}}
		end
	catch
		error : Reason ->
			{error, Reason};
		throw : Reason ->
			{error, Reason};
		exit : Reason ->
			{error, Reason}
	end.


commit_migration (Process, Token)
		when is_pid (Process) ->
	try
		case gen_server:call (Process, {mosaic_process, commit_migration, Token}, infinity) of
			Outcome = ok ->
				Outcome;
			Error = {error, _Reason} ->
				Error;
			Outcome ->
				{error, {invalid_outcome, Outcome}}
		end
	catch
		error : Reason ->
			{error, Reason};
		throw : Reason ->
			{error, Reason};
		exit : Reason ->
			{error, Reason}
	end.


rollback_migration (Process, Token)
		when is_pid (Process) ->
	try
		case gen_server:call (Process, {mosaic_process, rollback_migration, Token}, infinity) of
			Outcome = ok ->
				Outcome;
			Error = {error, _Reason} ->
				Error;
			Outcome ->
				{error, {invalid_outcome, Outcome}}
		end
	catch
		error : Reason ->
			{error, Reason};
		throw : Reason ->
			{error, Reason};
		exit : Reason ->
			{error, Reason}
	end.
