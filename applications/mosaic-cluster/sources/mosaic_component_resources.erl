
-module (mosaic_component_resources).


-export ([acquire/4, acquire/5]).


acquire (OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent) ->
	acquire (mosaic_component_resources, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent).

acquire (Delegate, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (OwnerIdentifier), (bit_size (OwnerIdentifier) =:= 160), (is_pid (OwnerProcess) orelse is_port (OwnerProcess)) ->
	try gen_server:call (Delegate, {mosaic_component_resources, acquire, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent}) of
		Outcome = {ok, _Descriptor} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.
