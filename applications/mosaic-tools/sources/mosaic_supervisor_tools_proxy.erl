
-module (mosaic_supervisor_tools_proxy).

-behaviour (supervisor).


-export ([init/1]).
-export ([start_link_port/2]).


init ({supervisor, QualifiedName, {RestartPolicy, ChildSpecifications}}) ->
	case mosaic_process_tools:enforce_registered (QualifiedName) of
		ok ->
			{ok, {RestartPolicy, ChildSpecifications}};
		{error, Reason} ->
			erlang:exit (Reason)
	end;
	
init ([Argument]) ->
	init (Argument);
	
init (Specification) ->
	erlang:exit ({invalid_specification, Specification, invalid_term}).


start_link_port (PortName, PortSettings)
		when is_tuple (PortName), is_list (PortSettings) ->
	Controller = erlang:spawn_link (
			fun () ->
				false = erlang:process_flag (trap_exit, true),
				Port = erlang:open_port (PortName, [exit_status | PortSettings]),
				receive
					{Port, Reason = {exit_status, _Status}} ->
						erlang:exit (Reason);
					{Port, Callback} ->
						erlang:exit ({unexpected_Callback, Callback});
					{'EXIT', Port, Reason} ->
						erlang:exit (Reason);
					{'EXIT', _Process, Reason} ->
						erlang:exit (Reason);
					Message ->
						erlang:exit ({unexpected_message, Message})
				end,
				erlang:exit (error)
			end),
	{ok, Controller}.
