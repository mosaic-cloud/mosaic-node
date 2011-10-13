
-module (mosaic_component_process_tests).


-export ([test/0]).
-export ([
		test_start_stop/1,
		test_call/1,
		test_cast/1,
		test_migrate/1,
		test_abacus/1]).
-export ([configure/6]).


-import (mosaic_process_tests, [start_link_process/4, stop_and_wait_process/1, wait_process/1, call_process/4, cast_process/4]).
-import (mosaic_process_migrator_tests, [start_link_process_migrator/6, wait_process_migrator/1]).
-import (mosaic_enforcements, [enforce_ok_1/1]).


-test ({test_start_stop, [{defaults}]}).
-test ({test_call, [{defaults}]}).
-test ({test_cast, [{defaults}]}).
-test ({test_migrate, [{defaults}]}).
-test ({test_abacus, [{python}]}).
-test ({test_abacus, [{node}]}).
-test ({test_abacus, [{java}]}).


test_start_stop ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure ('mosaic-tests:python-parrot', create, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	ok = stop_and_wait_process (Process),
	ok.


test_call ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure ('mosaic-tests:python-parrot', create, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	{ok, InputName1} = mosaic_generic_coders:generate_hex_data (8),
	{ok, InputValue1} = mosaic_generic_coders:generate_hex_data (8),
	Inputs = {struct, [{InputName1, InputValue1}]},
	{ok, Data} = mosaic_generic_coders:generate_data (8),
	{ok, Inputs, Data} = call_process (Process, <<"call-return">>, Inputs, Data),
	ok = stop_and_wait_process (Process),
	ok.


test_cast ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure ('mosaic-tests:python-parrot', create, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	{ok, InputName1} = mosaic_generic_coders:generate_hex_data (8),
	{ok, InputValue1} = mosaic_generic_coders:generate_hex_data (8),
	Inputs = {struct, [{InputName1, InputValue1}]},
	{ok, Data} = mosaic_generic_coders:generate_data (8),
	ok = cast_process (Process, <<"cast-mirror">>, Inputs, Data),
	ok = receive {'$gen_cast', {mosaic_process_router, cast, <<0 : 160>>, <<"cast-mirror">>, Inputs, Data}} -> ok end,
	ok = stop_and_wait_process (Process),
	ok.


test_migrate ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, SourceConfiguration} = configure ('mosaic-tests:python-parrot', create, Identifier),
	{ok, TargetConfiguration} = configure ('mosaic-tests:python-parrot', migrate, Identifier),
	Self = erlang:self (),
	SelfToken = erlang:make_ref (),
	SourceToken = erlang:make_ref (),
	TargetToken = erlang:make_ref (),
	{ok, Source} = start_link_process (mosaic_component_process, create, Identifier, SourceConfiguration),
	{ok, Target} = start_link_process (mosaic_component_process, {migrate, TargetToken}, Identifier, TargetConfiguration),
	{ok, Migrator} = start_link_process_migrator (Source, SourceToken, Target, TargetToken, Self, SelfToken),
	ok = mosaic_process_migrator:migrate (Migrator, defaults),
	ok = receive {mosaic_process_migrator, migrate, SelfToken, succeeded} -> ok end,
	ok = wait_process_migrator (Migrator),
	ok = wait_process (Source),
	ok = stop_and_wait_process (Target),
	ok.


test_abacus ({Flavour}) ->
	{ok, Type} = case Flavour of
		python ->
			{ok, 'mosaic-tests:python-abacus'};
		node ->
			{ok, 'mosaic-tests:node-abacus'};
		java ->
			{ok, 'mosaic-tests:java-abacus'}
	end,
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure (Type, create, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	{ok, Outcome, <<>>} = call_process (Process, <<"+">>, [1, 2], <<>>),
	ok = if Outcome == 3 -> ok end,
	ok = stop_and_wait_process (Process),
	ok.


configure (Type, create, Identifier) ->
	{ok, mosaic_component_process, Configuration} = configure (Type, create, Identifier, term, defaults, [{router, erlang:self ()}]),
	{ok, Configuration};
	
configure (Type, migrate, Identifier) ->
	{ok, mosaic_component_process, Configuration} = configure (Type, {migrate, target}, Identifier, term, defaults, [{router, erlang:self ()}]),
	{ok, Configuration}.


test () ->
	_ = mosaic_tests:test_module (mosaic_component_process_tests),
	ok = mosaic_application_tools:shutdown_async (0),
	ok.


configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, defaults) ->
	configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, []);
	
configure (Type, create, Identifier, term, OriginalConfiguration, ExtraOptions)
		when is_atom (Type) ->
	case configure_1 (Type, Identifier, OriginalConfiguration, ExtraOptions) of
		{ok, Options} ->
			case mosaic_component_process_coders:parse_configuration (create, term, Options) of
				{ok, Configuration} ->
					{ok, mosaic_component_process, Configuration};
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end;
	
configure (_Type, {migrate, source}, _Identifier, term, defaults, _ExtraOptions) ->
	{ok, none, defaults};
	
configure (Type, {migrate, target}, Identifier, term, defaults, ExtraOptions) ->
	Options = [
			{harness, [
				{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (migrate, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (_Type, _Disposition, _Identifier, term, Configuration, ExtraOptions)
		when is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}};
	
configure (Type, Disposition, Identifier, json, null, ExtraOptions)
		when is_list (ExtraOptions) ->
	configure (Type, Disposition, Identifier, term, defaults, ExtraOptions);
	
configure (Type, Disposition, Identifier, json, {struct, []}, ExtraOptions)
		when is_list (ExtraOptions) ->
	configure (Type, Disposition, Identifier, term, defaults, ExtraOptions);
	
configure (Type, Disposition, Identifier, json, Configuration, ExtraOptions)
		when is_list (ExtraOptions) ->
	configure (Type, Disposition, Identifier, term, {json, Configuration}, ExtraOptions).


configure_1 (Type = 'mosaic-tests:python-parrot', Identifier, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"python2">>))},
					{arguments, [
						<<Workbench / binary, "/applications/mosaic-harness/sources/mosaic_harness_tester.py">>,
						<<"backend">>, <<"parrot">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-tests:python-abacus', Identifier, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, <<Workbench / binary, "/../mosaic-python-components/.tools/bin/python">>},
					{arguments, [
						<<Workbench / binary, "/../mosaic-python-components/sources/examples/abacus.py">>]},
					{environment, [
						{<<"PYTHONPATH">>, <<Workbench / binary, "/../mosaic-python-components/sources/library">>}]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-tests:node-abacus', Identifier, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"node">>))},
					{arguments, [
						<<"./mosaic_component_abacus.js">>]},
					{working_directory, <<Workbench / binary, "/applications/mosaic-component/sources">>}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-tests:java-abacus', Identifier, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"java">>))},
					{arguments, [
						<<"-jar">>, <<Workbench / binary, "/../mosaic-java-components/components-examples/target/components-examples-abacus-0.2-SNAPSHOT-jar-with-dependencies.jar">>,
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-tests:java-component', Identifier, {json, JarPath}, ExtraOptions)
		when is_binary (JarPath), is_list (ExtraOptions) ->
	try
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"java">>))},
					{arguments, [
						<<"-jar">>, JarPath,
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-tests:java-component', Identifier, {json, [MainClass, ClassPath]}, ExtraOptions)
		when is_binary (MainClass), is_binary (ClassPath), is_list (ExtraOptions) ->
	try
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"java">>))},
					{arguments, [
						<<"-classpath">>, ClassPath,
						MainClass,
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, defaults, ExtraOptions)
		when ((Type =:= 'mosaic-tests:rabbitmq') orelse (Type =:= 'mosaic-components:rabbitmq')), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:rabbitmq' ->
				Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
				<<Workbench / binary, "/../mosaic-components-rabbitmq/scripts/run-component">>;
			'mosaic-components:rabbitmq' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-rabbitmq--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, defaults, ExtraOptions)
		when ((Type =:= 'mosaic-tests:riak-kv') orelse (Type =:= 'mosaic-components:riak-kv')), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:riak-kv' ->
				Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
				<<Workbench / binary, "/../mosaic-components-riak-kv/scripts/run-component">>;
			'mosaic-components:riak-kv' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-riak-kv--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, defaults, ExtraOptions)
		when ((Type =:= 'mosaic-tests:httpg') orelse (Type =:= 'mosaic-components:httpg')), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:httpg' ->
				Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
				<<Workbench / binary, "/../mosaic-components-httpg/scripts/run-component">>;
			'mosaic-components:httpg' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-httpg--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, {json, [MainClass, ClassPath]}, ExtraOptions)
		when ((Type =:= 'mosaic-tests:java-container') orelse (Type =:= 'mosaic-components:java-container')),
			is_binary (MainClass), is_binary (ClassPath), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:java-container' ->
				Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
				<<Workbench / binary, "/../mosaic-java-components/components-container/scripts/run-component">>;
			'mosaic-components:java-container' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-container--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						MainClass, ClassPath]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, {json, [MainClass, ClassPath, Logger]}, ExtraOptions)
		when ((Type =:= 'mosaic-tests:java-container') orelse (Type =:= 'mosaic-components:java-container')),
			is_binary (MainClass), is_binary (ClassPath), is_binary (Logger), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:java-container' ->
				Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
				<<Workbench / binary, "/../mosaic-java-components/components-container/scripts/run-component">>;
			'mosaic-components:java-container' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-container--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						MainClass, ClassPath, Logger]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-components:java-driver', Identifier, {json, Class}, ExtraOptions)
		when is_binary (Class), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-drivers--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						Class]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-components:java-cloudlet-container', Identifier, {json, [ClassPath, Descriptor]}, ExtraOptions)
		when is_binary (ClassPath), is_binary (Descriptor), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-cloudlet-container--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						ClassPath, Descriptor]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-tests:jetty-hello-world', Identifier, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Workbench = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_workbench">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"java">>))},
					{arguments, [
						<<"-jar">>, <<Workbench / binary, "/../mosaic-components-jetty/target/components-jetty-0.2-SNAPSHOT-jar-with-dependencies.jar">>,
						enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<Workbench / binary, "/../mosaic-components-jetty-examples/hello-world/target/components-jetty-examples-hello-world-0.2-SNAPSHOT.war">>]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, defaults, ExtraOptions)
		when ((Type =:= 'mosaic-examples-realtime-feeds:fetcher') orelse (Type =:= 'mosaic-examples-realtime-feeds:indexer') orelse
					(Type =:= 'mosaic-examples-realtime-feeds:scavanger') orelse (Type =:= 'mosaic-examples-realtime-feeds:leacher') orelse
					(Type =:= 'mosaic-examples-realtime-feeds:pusher') orelse (Type =:= 'mosaic-examples-realtime-feeds:frontend')),
				is_list (ExtraOptions) ->
	try
		<<"mosaic-examples-realtime-feeds:", TypeSuffix / binary>> = erlang:atom_to_binary (Type, utf8),
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-examples-realtime-feeds--run-", TypeSuffix / binary>>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-examples-realtime-feeds:java-indexer', Identifier, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		<<"mosaic-examples-realtime-feeds:", TypeSuffix / binary>> = erlang:atom_to_binary (Type, utf8),
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-examples-realtime-feeds-java--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, _Identifier, Configuration, _ExtraOptions)
		when is_atom (Type) ->
	{error, {invalid_type_or_configuration, {Type, Configuration}}}.
