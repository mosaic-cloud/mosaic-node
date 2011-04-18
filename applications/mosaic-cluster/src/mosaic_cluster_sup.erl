
-module (mosaic_cluster_sup).

-behaviour (supervisor).

-export ([start_link/0, start_link/1, start_link/2]).
-export ([
		start_child_process_controller/2, start_child_process_controller/3,
		start_child_process/3, start_child_process/4,
		start_child_object_store/2, start_child_object_store/3,
		start_child_daemon/4, start_child_daemon/5,
		start_child_vnode_master/1]).
-export ([init/1]).


start_link () ->
	start_link (mosaic_cluster_sup).

start_link (Type)
		when is_atom (Type) ->
	start_link (Type, Type).

start_link (Name, Type)
		when is_atom (Name), is_atom (Type) ->
	supervisor:start_link ({local, Name}, mosaic_cluster_sup, [Type]).


start_child_process_controller (Name, Configuration) ->
	start_child_process_controller (mosaic_process_controller_sup, Name, Configuration).

start_child_process_controller (Supervisor, Name, Configuration)
		when (is_pid (Supervisor) or is_atom (Supervisor)), is_atom (Name) ->
	start_child (Supervisor, [Name, Configuration]).


start_child_process (Name, Module, Disposition) ->
	start_child_process (mosaic_process_sup, Name, Module, Disposition).

start_child_process (Supervisor, Name, Module, Disposition)
		when (is_pid (Supervisor) or is_atom (Supervisor)), is_atom (Name), is_atom (Module), is_tuple (Disposition) ->
	start_child (Supervisor, [Name, Module, Disposition]).


start_child_object_store (Name, Configuration) ->
	start_child_object_store (mosaic_object_store_sup, Name, Configuration).

start_child_object_store (Supervisor, Name, Configuration)
		when (is_pid (Supervisor) or is_atom (Supervisor)), is_atom (Name) ->
	start_child (Supervisor, [Name, Configuration]).


start_child_daemon (Name, Module, Arguments, Policy) ->
	start_child_daemon (mosaic_daemon_sup, Name, Module, Arguments, Policy).

start_child_daemon (Supervisor, Name, Module, Arguments, Policy)
		when (is_pid (Supervisor) or is_atom (Supervisor)), is_atom (Name), is_atom (Module),
				(Policy =:= permanent) or (Policy =:= transient) or (Policy =:= temporary) ->
	case child_spec ({worker, Policy}, Name, Module, start_link, [Name | Arguments]) of
		{ok, Specification} ->
			start_child (Supervisor, Specification);
		Error = {error, _Reason} ->
			Error
	end.


start_child_vnode_master (Module)
		when is_atom (Module) ->
	Name = riak_core_vnode_master:reg_name (Module),
	case child_spec ({worker, permanent}, Name, riak_core_vnode_master, start_link, [Module]) of
		{ok, Specification} ->
			start_child (mosaic_vnode_master_sup, Specification);
		Error = {error, _Reason} ->
			Error
	end.


start_child (Supervisor, Specification) ->
	case supervisor:start_child (Supervisor, Specification) of
		{ok, Child} ->
			{ok, Child};
		{ok, Child, _Data} ->
			{ok, Child};
		ignore ->
			{error, ignore};
		{error, {already_started, _Child}} ->
			{error, already_started};
		{error, already_present} ->
			{error, already_started};
		{error, {Error, _Specification}} ->
			{error, Error}
	end.


init ([mosaic_cluster_sup]) ->
	{ok, {{one_for_all, 1, 60}, [
		child_spec_unwrapped (supervisor, mosaic_process_controller_sup, supervisor, start_link,
				[{local, mosaic_process_controller_sup}, mosaic_cluster_sup, [mosaic_process_controller_sup]]),
		child_spec_unwrapped (supervisor, mosaic_process_sup, supervisor, start_link,
				[{local, mosaic_process_sup}, mosaic_cluster_sup, [mosaic_process_sup]]),
		child_spec_unwrapped (supervisor, mosaic_object_store_sup, supervisor, start_link,
				[{local, mosaic_object_store_sup}, mosaic_cluster_sup, [mosaic_object_store_sup]]),
		child_spec_unwrapped (supervisor, mosaic_daemon_sup, supervisor, start_link,
				[{local, mosaic_daemon_sup}, mosaic_cluster_sup, [mosaic_daemon_sup]]),
		child_spec_unwrapped (supervisor, mosaic_vnode_master_sup, supervisor, start_link,
				[{local, mosaic_vnode_master_sup}, mosaic_cluster_sup, [mosaic_vnode_master_sup]])]}};
	
init ([mosaic_process_controller_sup]) ->
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec_unwrapped ({worker, temporary}, undefined, mosaic_process_controller, start_link, [])]}};
	
init ([mosaic_process_sup]) ->
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec_unwrapped ({worker, temporary}, undefined, mosaic_process, start_link, [])]}};
	
init ([mosaic_object_store_sup]) ->
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec_unwrapped ({worker, temporary}, undefined, mosaic_object_store, start_link, [])]}};
	
init ([mosaic_daemon_sup]) ->
	{ok, {{one_for_one, 60, 60}, []}};
	
init ([mosaic_vnode_master_sup]) ->
	{ok, {{one_for_one, 1, 60}, []}}.


child_spec (Type, Identifier, Module, StartLinkFunction, StartLinkArguments)
		when is_atom (Module), is_atom (StartLinkFunction), is_list (StartLinkArguments) ->
	ModuleFunctionArguments = {Module, StartLinkFunction, StartLinkArguments},
	Specification = case Type of
		supervisor ->
			{Identifier, ModuleFunctionArguments, permanent, 60 * 1000, supervisor, dynamic};
		{worker, Restart} ->
			{Identifier, ModuleFunctionArguments, Restart, 60 * 1000, worker, dynamic}
	end,
	{ok, Specification}.

child_spec_unwrapped (Type, Identifier, Module, StartLinkFunction, StartLinkArguments) ->
	{ok, Specification} = child_spec (Type, Identifier, Module, StartLinkFunction, StartLinkArguments),
	Specification.
