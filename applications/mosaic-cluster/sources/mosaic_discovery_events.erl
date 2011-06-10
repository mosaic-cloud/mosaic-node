
-module (mosaic_discovery_events).

-behaviour (gen_event).


-export ([start/0, start/1, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/0]).
-export ([stop/0, stop/1, stop/2]).
-export ([register_handler/2, register_handler/3]).
-export ([broadcasted/1, broadcasted/2]).
-export ([init/1, terminate/2, code_change/3, handle_event/2, handle_call/2, handle_info/2]).


start () ->
	start (noname).

start (QualifiedName) ->
	mosaic_process_tools:start (gen_event, none, QualifiedName, void).


start_link () ->
	start_link (noname).

start_link (QualifiedName) ->
	mosaic_process_tools:start_link (gen_event, none, QualifiedName, void).

start_link (QualifiedName, void) ->
	start_link (QualifiedName).


start_supervised () ->
	mosaic_sup:start_child_daemon (mosaic_discovery_events, {local, mosaic_discovery_events}, [void], permanent).


stop () ->
	stop (mosaic_discovery_events).

stop (Events) ->
	stop (Events, normal).

stop (Events, Signal)
		when (is_pid (Events) orelse is_atom (Events)) ->
	gen_event:call (Events, {mosaic_discrovery_events, stop, Signal}).


register_handler (Module, Arguments) ->
	register_handler (mosaic_discovery_events, Module, Arguments).

register_handler (Events, Module, Arguments)
		when (is_pid (Events) orelse is_atom (Events)), is_atom (Module) ->
	case gen_event:add_handler (Events, Module, Arguments) of
		ok ->
			ok;
		{'EXIT', Reason} ->
			{error, Reason};
		Outcome ->
			{error, {invalid_outcome, Outcome}}
	end.


broadcasted (Message) ->
	gen_event:notify (mosaic_discovery_events, Message).

broadcasted (Events, Message)
		when (is_pid (Events) orelse is_atom (Events)) ->
	gen_event:notify (Events, {mosaic_discovery_events, broadcasted, Message}).


init (Closure = {Function, _State})
		when is_function (Function, 2) ->
	{ok, Closure}.

terminate (_Arguments, _Closure) ->
	ok.

code_change (_OldVsn, Closure, _Arguments) ->
	{ok, Closure}.

handle_event (Event, {Function, OldState}) ->
	{ok, NewState} = Function (Event, OldState),
	{ok, {Function, NewState}}.

handle_call (Request, Closure) ->
	ok = mosaic_transcript:trace_error ("received invalid call request; ignoring!", [{request, Request}]),
	{ok, {error, {invalid_request, Request}}, Closure}.

handle_info (Message, Closure) ->
	ok = mosaic_transcript:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{ok, Closure}.
