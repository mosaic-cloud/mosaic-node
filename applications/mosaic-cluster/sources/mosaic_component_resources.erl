
-module (mosaic_component_resources).


-export ([acquire/3, acquire/4]).


acquire (OwnerIdentifier, OwnerProcess, Specification) ->
	acquire (mosaic_component_resources, OwnerIdentifier, OwnerProcess, Specification).

acquire (Delegate, OwnerIdentifier, OwnerProcess, Specifications)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (OwnerIdentifier), (bit_size (OwnerIdentifier) =:= 160),
				(is_pid (OwnerProcess) orelse is_port (OwnerProcess)) ->
	try gen_server:call (Delegate, {mosaic_component_resources, acquire, OwnerIdentifier, OwnerProcess, Specifications}) of
		Outcome = {ok, _Descriptor} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.
