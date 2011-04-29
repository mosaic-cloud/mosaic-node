
-module (mosaic_cluster_sup).

-behaviour (supervisor).

-export ([start_link/0, start_link/1, start_link/2]).
-export ([
		start_child_process_controller/2, start_child_process_controller/3,
		start_child_process/3, start_child_process/4,
		start_child_object_store/2, start_child_object_store/3,
		start_child_daemon/4, start_child_daemon/5, start_child_daemon/6,
		start_child_vnode_master/1]).
-export ([init/1]).


start_link () ->
	start_link (mosaic_cluster_sup).

start_link (Type)
		when is_atom (Type) ->
	start_link ({local, Type}, Type).

start_link (QualifiedName = {local, LocalName}, Type)
		when is_atom (LocalName), is_atom (Type) ->
	supervisor:start_link (QualifiedName, mosaic_cluster_sup, [QualifiedName, Type]);
	
start_link (QualifiedName = noname, Type)
		when is_atom (Type) ->
	supervisor:start_link (mosaic_cluster_sup, [QualifiedName, Type]).


start_child_process_controller (QualifiedName, Configuration) ->
	start_child_process_controller (mosaic_process_controller_sup, QualifiedName, Configuration).

start_child_process_controller (Supervisor, QualifiedName, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	start_child (Supervisor, [QualifiedName, Configuration]).


start_child_process (QualifiedName, Module, Disposition) ->
	start_child_process (mosaic_process_sup, QualifiedName, Module, Disposition).

start_child_process (Supervisor, QualifiedName, Module, Disposition)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				is_atom (Module), (is_record (Disposition, create, 2) orelse is_record (Disposition, migrate, 2)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	start_child (Supervisor, [QualifiedName, Module, Disposition]).


start_child_object_store (QualifiedName, Configuration) ->
	start_child_object_store (mosaic_object_store_sup, QualifiedName, Configuration).

start_child_object_store (Supervisor, QualifiedName, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	start_child (Supervisor, [QualifiedName, Configuration]).


start_child_daemon (QualifiedName, Module, Arguments, Policy) ->
	start_child_daemon (mosaic_daemon_sup, QualifiedName, Module, Arguments, Policy).

start_child_daemon (Supervisor, QualifiedName, Module, Arguments, Policy) ->
	start_child_daemon (Supervisor, QualifiedName, Module, start_link, Arguments, Policy).

start_child_daemon (Supervisor, QualifiedName, Module, StartLinkFunction, Arguments, Policy)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)), is_atom (Module), is_atom (StartLinkFunction), is_list (Arguments),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))),
				(Policy =:= permanent) orelse (Policy =:= transient) orelse (Policy =:= temporary) ->
	{ok, StartLinkArguments} = if
		QualifiedName =:= noname ->
			{ok, Arguments};
		true ->
			{ok, [QualifiedName | Arguments]}
	end,
	case child_spec ({worker, Policy}, QualifiedName, Module, StartLinkFunction, StartLinkArguments) of
		{ok, Specification} ->
			start_child (Supervisor, Specification);
		Error = {error, _Reason} ->
			Error
	end.


start_child_vnode_master (Module)
		when is_atom (Module) ->
	LocalName = riak_core_vnode_master:reg_name (Module),
	case child_spec ({worker, permanent}, LocalName, riak_core_vnode_master, start_link, [Module]) of
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
		{error, {Error = {error, _Reason}, _Specification}} ->
			Error;
		Error = {error, _Reason} ->
			Error
	end.


init ([Type])
		when is_atom (Type) ->
	init ([{local, Type}, Type]);
	
init ([QualifiedName, mosaic_cluster_sup]) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
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
	
init ([QualifiedName, mosaic_process_controller_sup]) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec_unwrapped ({worker, temporary}, undefined, mosaic_process_controller, start_link, [])]}};
	
init ([QualifiedName, mosaic_process_sup]) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec_unwrapped ({worker, temporary}, undefined, mosaic_process, start_link, [])]}};
	
init ([QualifiedName, mosaic_object_store_sup]) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec_unwrapped ({worker, temporary}, undefined, mosaic_object_store, start_link, [])]}};
	
init ([QualifiedName, mosaic_daemon_sup]) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
	{ok, {{one_for_one, 60, 60}, []}};
	
init ([QualifiedName, mosaic_vnode_master_sup]) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
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
