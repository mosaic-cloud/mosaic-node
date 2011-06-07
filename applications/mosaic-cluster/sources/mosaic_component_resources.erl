
-module (mosaic_component_resources).


-export ([acquire/4, acquire/5]).


acquire (OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent) ->
	acquire (mosaic_component_resources, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent).

acquire (Manager, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent)
		when (is_pid (Manager) orelse is_atom (Manager)), is_binary (OwnerIdentifier), (bit_size (OwnerIdentifier) =:= 160), (is_pid (OwnerProcess) orelse is_port (OwnerProcess)) ->
	try gen_server:call (Manager, {mosaic_component_resources, acquire, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent}) of
		Outcome = {ok, _Descriptor} ->
			Outcome;
		Error = {error, _Reason} ->
			Error
	catch
		error : Reason ->
			{error, Reason};
		throw : Reason ->
			{error, Reason};
		exit : Reason ->
			{error, Reason}
	end.
