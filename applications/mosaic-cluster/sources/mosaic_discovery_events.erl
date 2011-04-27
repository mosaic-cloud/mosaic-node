
-module (mosaic_discovery_events).

-behaviour (gen_event).

-export ([start/0, start/1, start_link/0, start_link/1]).
-export ([start_supervised/0, start_supervised/1, start_supervised/2]).
-export ([register_handler/2, register_handler/3]).
-export ([stop/1]).
-export ([broadcasted/2]).
-export ([init/1, terminate/2, code_change/3, handle_event/2, handle_call/2, handle_info/2]).


start () ->
	start (noname).

start (QualifiedName = {local, LocalName})
		when is_atom (LocalName) ->
	gen_event:start (QualifiedName);
	
start (noname) ->
	gen_event:start ().


start_link () ->
	start_link (noname).

start_link (QualifiedName = {local, LocalName})
		when is_atom (LocalName) ->
	gen_event:start_link (QualifiedName);
	
start_link (noname) ->
	gen_event:start_link ().


start_supervised () ->
	start_supervised ({local, mosaic_discovery_events}).

start_supervised (QualifiedName) ->
	start_supervised (mosaic_daemon_sup, QualifiedName).

start_supervised (Supervisor, QualifiedName) ->
	mosaic_cluster_sup:start_child_daemon (Supervisor, QualifiedName, mosaic_discovery_events, [], permanent).


register_handler (Module, Arguments) ->
	register_handler (mosaic_discovery_events, Module, Arguments).

register_handler (Events, Module, Arguments)
		when (is_atom (Events) orelse is_pid (Events)), is_atom (Module) ->
	gen_event:add_handler (Events, Module, Arguments).


stop (Events)
		when (is_atom (Events) orelse is_pid (Events)) ->
	gen_event:stop (Events).


broadcasted (Events, Message)
		when (is_atom (Events) orelse is_pid (Events)) ->
	gen_event:notify (Events, {broadcasted, Message}).


init (Closure = {Function, _State})
		when is_function (Function, 2) ->
	{ok, Closure}.

terminate (_Arguments, _Closure) ->
	ok.

code_change (_OldVsn, Closure, _Data) ->
	{ok, Closure}.

handle_event (Event, {Function, OldState}) ->
	{ok, NewState} = Function (Event, OldState),
	{ok, {Function, NewState}}.

handle_call ({stop, normal}, _Closure) ->
	{remove_handler, ok}.

handle_info ({stop, normal}, _Closure) ->
	remove_handler.
