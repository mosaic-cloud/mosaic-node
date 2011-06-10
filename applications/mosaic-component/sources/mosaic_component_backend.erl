
-module (mosaic_component_backend).


-export ([behaviour_info/1]).
-export ([terminate/0, call/4, cast/4, call_return/2, register/1, acquire/1]).


behaviour_info (callbacks) ->
	[
		{configure, 0},
		{start_link, 0}].


terminate () ->
	gen_server:call (mosaic_component, {mosaic_component, terminate}).


call (Component, Operation, Inputs, Data)
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (Operation), is_binary (Data) ->
	gen_server:call (mosaic_component_backend, {mosaic_component_backend, call, Component, Operation, Inputs, Data}).


cast (Component, Operation, Inputs, Data)
		when is_binary (Component), (bit_size (Component) =:= 160), is_binary (Operation), is_binary (Data) ->
	ok = gen_server:cast (mosaic_component_backend, {mosaic_component_backend, cast, Component, Operation, Inputs, Data}).


call_return ({Sender, Reference}, Outcome)
		when (is_pid (Sender) orelse is_atom (Sender)), is_reference (Reference) ->
	ok = gen_server:reply (Sender, {mosaic_component_backend, call_return, Reference, Outcome}).


register (Group)
		when is_binary (Group), (bit_size (Group) =:= 160) ->
	gen_server:call (mosaic_component_backend, {mosaic_component_backend, register, Group}).


acquire (ResourceSpecifications) ->
	gen_server:call (mosaic_component_backend, {mosaic_component_backend, acquire, ResourceSpecifications}).
