
-module (mosaic_sup).

-behaviour (supervisor).

-export ([start_link/0, start_link/1, start_link/2]).
-export ([
		start_child_process/5, start_child_process/6,
		start_child_process_controller/2, start_child_process_controller/3,
		start_child_object_store/2, start_child_object_store/3,
		start_child_daemon/4, start_child_daemon/5, start_child_daemon/6,
		start_child_vnode_master/1]).
-export ([init/1]).


start_link () ->
	start_link (mosaic_sup).

start_link (Type) ->
	start_link ({local, Type}, Type).

start_link (QualifiedName = {local, LocalName}, Type)
		when is_atom (LocalName), is_atom (Type) ->
	supervisor:start_link (QualifiedName, mosaic_sup, [{QualifiedName, Type}]);
	
start_link (QualifiedName = noname, Type)
		when is_atom (Type) ->
	supervisor:start_link (mosaic_sup, [{QualifiedName, Type}]).


start_child_process (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	start_child_process (mosaic_process_sup, QualifiedName, Module, Disposition, Identifier, Configuration).

start_child_process (Supervisor, QualifiedName, Module, Disposition, Identifier, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)), is_atom (Module),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))),
				((Disposition =:= create) orelse (is_record (Disposition, migrate, 2) andalso is_reference (element (2, Disposition)))),
				is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	start_child (Supervisor, [QualifiedName, Module, Disposition, Identifier, Configuration]).


start_child_process_controller (QualifiedName, Configuration) ->
	start_child_process_controller (mosaic_process_controller_sup, QualifiedName, Configuration).

start_child_process_controller (Supervisor, QualifiedName, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	start_child (Supervisor, [QualifiedName, Configuration]).


start_child_object_store (QualifiedName, Configuration) ->
	start_child_object_store (mosaic_object_store_sup, QualifiedName, Configuration).

start_child_object_store (Supervisor, QualifiedName, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	start_child (Supervisor, [QualifiedName, Configuration]).


start_child_daemon (Module, QualifiedName, Arguments, Policy) ->
	start_child_daemon (mosaic_daemon_sup, Module, QualifiedName, Arguments, Policy).

start_child_daemon (Supervisor, Module, QualifiedName, Arguments, Policy) ->
	start_child_daemon (Supervisor, Module, QualifiedName, start_link, Arguments, Policy).

start_child_daemon (Supervisor, Module, QualifiedName, StartLinkFunction, Arguments, Policy)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)), is_atom (Module), is_atom (StartLinkFunction), is_list (Arguments),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))),
				((Policy =:= permanent) orelse (Policy =:= transient) orelse (Policy =:= temporary)) ->
	StartLinkArguments = if
		QualifiedName =:= noname ->
			Arguments;
		true ->
			[QualifiedName | Arguments]
	end,
	Specification = child_spec ({worker, Policy}, QualifiedName, Module, StartLinkFunction, StartLinkArguments),
	start_child (Supervisor, Specification).


start_child_vnode_master (Module)
		when is_atom (Module) ->
	LocalName = riak_core_vnode_master:reg_name (Module),
	Specification = child_spec ({worker, permanent}, {local, LocalName}, riak_core_vnode_master, start_link, [Module]),
	start_child (mosaic_vnode_master_sup, Specification).


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


init ([{Type}])
		when is_atom (Type) ->
	init ([{{local, Type}, Type}]);
	
init ([{QualifiedName, mosaic_sup}]) ->
	ok = mosaic_tools:enforce_registered (QualifiedName),
	{ok, {{one_for_all, 1, 60}, [
		child_spec (supervisor, mosaic_process_sup, supervisor, start_link,
				[{local, mosaic_process_sup}, mosaic_sup, [{mosaic_process_sup}]]),
		child_spec (supervisor, mosaic_process_controller_sup, supervisor, start_link,
				[{local, mosaic_process_controller_sup}, mosaic_sup, [{mosaic_process_controller_sup}]]),
		child_spec (supervisor, mosaic_object_store_sup, supervisor, start_link,
				[{local, mosaic_object_store_sup}, mosaic_sup, [{mosaic_object_store_sup}]]),
		child_spec (supervisor, mosaic_daemon_sup, supervisor, start_link,
				[{local, mosaic_daemon_sup}, mosaic_sup, [{mosaic_daemon_sup}]]),
		child_spec (supervisor, mosaic_vnode_master_sup, supervisor, start_link,
				[{local, mosaic_vnode_master_sup}, mosaic_sup, [{mosaic_vnode_master_sup}]])]}};
	
init ([{QualifiedName, mosaic_process_sup}]) ->
	ok = mosaic_tools:enforce_registered (QualifiedName),
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec ({worker, temporary}, undefined, mosaic_process, start_link, [])]}};
	
init ([{QualifiedName, mosaic_process_controller_sup}]) ->
	ok = mosaic_tools:enforce_registered (QualifiedName),
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec ({worker, temporary}, undefined, mosaic_process_controller, start_link, [])]}};
	
init ([{QualifiedName, mosaic_object_store_sup}]) ->
	ok = mosaic_tools:enforce_registered (QualifiedName),
	{ok, {{simple_one_for_one, 60, 60}, [
			child_spec ({worker, temporary}, undefined, mosaic_object_store, start_link, [])]}};
	
init ([{QualifiedName, mosaic_daemon_sup}]) ->
	ok = mosaic_tools:enforce_registered (QualifiedName),
	{ok, {{one_for_one, 60, 60}, []}};
	
init ([{QualifiedName, mosaic_vnode_master_sup}]) ->
	ok = mosaic_tools:enforce_registered (QualifiedName),
	{ok, {{one_for_one, 1, 60}, []}}.


child_spec (Type, Identifier, Module, StartLinkFunction, StartLinkArguments)
		when is_atom (Module), is_atom (StartLinkFunction), is_list (StartLinkArguments) ->
	ModuleFunctionArguments = {Module, StartLinkFunction, StartLinkArguments},
	case Type of
		supervisor ->
			{Identifier, ModuleFunctionArguments, permanent, 60 * 1000, supervisor, dynamic};
		{worker, Restart} ->
			{Identifier, ModuleFunctionArguments, Restart, 60 * 1000, worker, dynamic}
	end.
