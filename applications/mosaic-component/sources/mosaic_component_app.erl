
-module (mosaic_component_app).

-behaviour (application).


-export ([start/2, stop/1, boot/0, shutdown/0]).


-import (mosaic_enforcements, [enforce_ok/1]).


start (normal, defaults) ->
	mosaic_component_sup:start_link ();
	
start (normal, Configuration) ->
	{error, {invalid_configuration, Configuration}};
	
start (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


stop (void) ->
	ok.


boot () ->
	try
		ok = enforce_ok (application:load (crypto)),
		ok = enforce_ok (application:load (mosaic_tools)),
		ok = enforce_ok (application:load (mosaic_harness)),
		ok = enforce_ok (application:load (mosaic_component)),
		ok = enforce_ok (mosaic_component_backend:configure ()),
		ok = enforce_ok (application:start (crypto)),
		ok = enforce_ok (application:start (mosaic_tools)),
		ok = enforce_ok (application:start (mosaic_harness)),
		ok = enforce_ok (application:start (mosaic_component)),
		ok
	catch
		throw : {error, Reason} ->
			ok = error_logger:error_report (["failed booting mosaic component; terminating!", {reason, Reason}, {stacktrace, erlang:get_stacktrace ()}]),
			shutdown ();
		_ : Reason ->
			ok = error_logger:error_report (["failed booting mosaic component; terminating!", {reason, Reason}, {stacktrace, erlang:get_stacktrace ()}]),
			shutdown ()
	end.


shutdown () ->
	ok = init:stop (),
	ok = timer:sleep (5000),
	ok = erlang:halt (),
	ok.
