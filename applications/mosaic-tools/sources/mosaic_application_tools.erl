
-module (mosaic_application_tools).


-export ([boot/1, shutdown_async/1, shutdown_async/2]).
-export ([load/1, load/2, start/1, start/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


boot (Application)
		when is_atom (Application) ->
	boot (
				fun () ->
					try
						ok = enforce_ok (load (Application, with_dependencies)),
						ok = enforce_ok (start (Application, with_dependencies)),
						ok
					catch throw : {error, Reason} -> {error, {failed_booting_application, Application, Reason}} end
				end);
	
boot (Booter)
		when is_function (Booter, 0) ->
	try
		ok = enforce_ok (Booter ()),
		ok
	catch
		throw : {error, Reason} ->
			ok = error_logger:error_report (["failed booting...", {reason, Reason}, {stacktrace, erlang:get_stacktrace ()}]),
			ok = shutdown_async (0),
			{error, {failed_booting, Reason}};
		_ : Reason ->
			ok = error_logger:error_report (["failed booting...", {reason, Reason}, {stacktrace, erlang:get_stacktrace ()}]),
			ok = shutdown_async (0),
			{error, {failed_booting, Reason}}
	end.


shutdown_async (StopDelay) ->
	shutdown_async (StopDelay, 6 * 1000).

shutdown_async (StopDelay, HaltDelay)
		when is_integer (StopDelay), (StopDelay >= 0), is_integer (HaltDelay), (HaltDelay >= 0) ->
	_ = erlang:spawn (
				fun () ->
					ok = timer:sleep (StopDelay),
					ok = error_logger:warning_report (["stopping..."]),
					ok = init:stop (),
					ok = timer:sleep (HaltDelay),
					ok = error_logger:warning_report (["halting..."]),
					ok = erlang:halt (),
					ok
				end),
	ok.


load (Application) ->
	load (Application, with_dependencies).

load (Application, Depth) ->
	control (Application, load, Depth).


start (Application) ->
	start (Application, with_dependencies).

start (Application, Depth) ->
	control (Application, start, Depth).


control ([], _Stage, _Depth) ->
	ok;
	
control ([Application | RemainingApplications], Stage, Depth)
		when is_atom (Application) ->
	try enforce_ok (control (Application, Stage, Depth)) of
		ok ->
			control (RemainingApplications, Stage, Depth)
	catch throw : Error = {error, _Reason} -> Error end;
	
control (Application, load, Depth)
		when is_atom (Application) ->
	try
		ok = case application:load (Application) of
			ok ->
				ok;
			{error, {already_loaded, Application}} ->
				ok;
			Error = {error, _Reason} ->
				throw (Error)
		end,
		ok = enforce_ok (control_dependencies (Application, load, Depth)),
		ok
	catch throw : {error, Reason} -> {error, {failed_loading, Application, Reason}} end;
	
control (Application, start, Depth)
		when is_atom (Application) ->
	try
		ok = case application:get_key (Application, applications) of
			{ok, _} ->
				ok;
			undefined ->
				throw ({error, not_loaded})
		end,
		ok = enforce_ok (control_dependencies (Application, start, Depth)),
		ok = case application:start (Application) of
			ok ->
				ok;
			{error, {already_started, Application}} ->
				ok;
			Error = {error, _Reason} ->
				throw (Error)
		end,
		ok
	catch throw : {error, Reason} -> {error, {failed_starting, Application, Reason}} end.


control_dependencies (Application, Stage, Depth)
		when is_atom (Application) ->
	try
		ok = case Depth of
			with_dependencies ->
				case application:get_key (Application, applications) of
					{ok, DependencyApplications} ->
						ok = enforce_ok (control (DependencyApplications, Stage, Depth)),
						ok;
					undefined ->
						throw ({error, missing_dependencies})
				end;
			without_dependencies ->
				ok
		end,
		ok
	catch throw : Error = {error, _Reason} -> Error end.
