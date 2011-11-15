
-module (mosaic_supervisor_tools_proxy).

-behaviour (supervisor).


-export ([init/1]).


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
