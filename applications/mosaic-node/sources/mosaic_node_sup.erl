
-module (mosaic_node_sup).


-export ([start_link/0, start_link/1]).
-export ([
		start_child_process/5, start_child_process/6,
		start_child_process_controller/2, start_child_process_controller/3,
		start_child_object_store/2, start_child_object_store/3,
		start_child_daemon/2, start_child_daemon/3, start_child_daemon/4, start_child_daemon/5,
		start_child_vnode_master/1]).


start_link () ->
	start_link (mosaic_node_sup).

start_link (Type)
		when is_atom (Type) ->
	{supervisor, QualifiedName, Configuration} = configuration (Type),
	mosaic_supervisor_tools:start_link (QualifiedName, Configuration).


configuration (mosaic_node_sup) ->
	{supervisor, {local, mosaic_node_sup}, {one_for_all, [
			configuration (mosaic_process_sup),
			configuration (mosaic_process_controller_sup),
			configuration (mosaic_object_store_sup),
			configuration (mosaic_daemon_sup),
			configuration (mosaic_vnode_master_sup)]}};
	
configuration (Type)
		when ((Type =:= mosaic_process_sup) orelse (Type =:= mosaic_process_controller_sup)
					orelse (Type =:= mosaic_object_store_sup) orelse (Type =:= mosaic_daemon_sup)
					orelse (Type =:= mosaic_vnode_master_sup)) ->
	{supervisor, {local, Type}, {one_for_one, []}, [{identifier, Type}]}.


start_child_process (QualifiedName, Module, Disposition, Identifier, Configuration) ->
	start_child_process (mosaic_process_sup, QualifiedName, Module, Disposition, Identifier, Configuration).

start_child_process (Supervisor, QualifiedName, Module, Disposition, Identifier, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)), is_atom (Module),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))),
				((Disposition =:= create) orelse (is_record (Disposition, migrate, 2) andalso is_reference (element (2, Disposition)))),
				is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	mosaic_supervisor_tools:start_child_process (Supervisor, QualifiedName, mosaic_process, {Module, Disposition, Identifier, Configuration}, []).


start_child_process_controller (QualifiedName, Configuration) ->
	start_child_process_controller (mosaic_process_controller_sup, QualifiedName, Configuration).

start_child_process_controller (Supervisor, QualifiedName, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	mosaic_supervisor_tools:start_child_process (Supervisor, QualifiedName, mosaic_process_controller, Configuration, []).


start_child_object_store (QualifiedName, Configuration) ->
	start_child_object_store (mosaic_object_store_sup, QualifiedName, Configuration).

start_child_object_store (Supervisor, QualifiedName, Configuration)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	mosaic_supervisor_tools:start_child_process (Supervisor, QualifiedName, mosaic_object_store, Configuration, []).


start_child_daemon (Module, Configuration) ->
	start_child_daemon (noname, Module, Configuration).

start_child_daemon (QualifiedName, Module, Configuration) ->
	start_child_daemon (mosaic_daemon_sup, QualifiedName, Module, Configuration).

start_child_daemon (Supervisor, QualifiedName, Module, Configuration) ->
	start_child_daemon (Supervisor, QualifiedName, Module, Configuration, []).

start_child_daemon (Supervisor, QualifiedName, Module, Configuration, Options)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)), is_atom (Module), is_list (Options),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	mosaic_supervisor_tools:start_child_process (Supervisor, QualifiedName, Module, Configuration, Options).


start_child_vnode_master (Module)
		when is_atom (Module) ->
	LocalName = riak_core_vnode_master:reg_name (Module),
	Specification = {LocalName, {riak_core_vnode_master, start_link, [Module]}, permanent, 12 * 1000, worker, dynamic},
	mosaic_supervisor_tools:start_child (mosaic_vnode_master_sup, Specification).
