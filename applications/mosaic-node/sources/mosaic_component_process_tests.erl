
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


configure_1 (Type, Identifier, defaults, ExtraOptions)
		when ((Type =:= 'mosaic-tests:rabbitmq') orelse (Type =:= 'mosaic-components:rabbitmq')), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:rabbitmq' ->
				Repositories = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_repositories">>)),
				<<Repositories / binary, "/mosaic-components-rabbitmq/scripts/run-component">>;
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
				Repositories = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_repositories">>)),
				<<Repositories / binary, "/mosaic-components-riak-kv/scripts/run-component">>;
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
				Repositories = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_repositories">>)),
				<<Repositories / binary, "/mosaic-components-httpg/scripts/run-component">>;
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
	
configure_1 (Type, Identifier, {json, [Class, Configuration, ClassPath]}, ExtraOptions)
		when ((Type =:= 'mosaic-tests:java-component-container') orelse (Type =:= 'mosaic-components:java-component-container')),
			is_binary (Class), is_list (ClassPath), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:java-component-container' ->
				Repositories = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_repositories">>)),
				<<Repositories / binary, "/mosaic-java-platform/components-container/scripts/run-component">>;
			'mosaic-components:java-component-container' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-component-container--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-class">>, Class,
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration)),
						<<"--component-classpath">>, erlang:list_to_binary (string:join (lists:map (fun erlang:binary_to_list/1, ClassPath), "|"))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, {json, [Resource, Configuration, ClassPath]}, ExtraOptions)
		when ((Type =:= 'mosaic-tests:java-driver-container') orelse (Type =:= 'mosaic-components:java-driver-container')),
				is_binary (Resource), is_list (ClassPath), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:java-driver-container' ->
				Repositories = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_repositories">>)),
				<<Repositories / binary, "/mosaic-java-platform/drivers/scripts/run-component">>;
			'mosaic-components:java-driver-container' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-driver-container--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						Resource,
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration)),
						<<"--component-classpath">>, erlang:list_to_binary (string:join (lists:map (fun erlang:binary_to_list/1, ClassPath), "|"))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, {json, [Configuration, ClassPath]}, ExtraOptions)
		when ((Type =:= 'mosaic-tests:java-cloudlet-container') orelse (Type =:= 'mosaic-components:java-cloudlet-container')),
				is_list (ClassPath), is_list (ExtraOptions) ->
	try
		Executable = case Type of
			'mosaic-tests:java-cloudlet-container' ->
				Repositories = enforce_ok_1 (mosaic_generic_coders:os_env_get (<<"_mosaic_repositories">>)),
				<<Repositories / binary, "/../mosaic-java-platform/cloudlets/scripts/run-component">>;
			'mosaic-components:java-cloudlet-container' ->
				enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-cloudlet-container--run-component">>))
		end,
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration)),
						<<"--component-classpath">>, erlang:list_to_binary (string:join (lists:map (fun erlang:binary_to_list/1, ClassPath), "|"))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, Identifier, defaults, ExtraOptions)
		when ((Type =:= 'mosaic-examples-realtime-feeds:fetcher') orelse (Type =:= 'mosaic-examples-realtime-feeds:indexer') orelse
					(Type =:= 'mosaic-examples-realtime-feeds:scavanger') orelse (Type =:= 'mosaic-examples-realtime-feeds:leacher') orelse (Type =:= 'mosaic-examples-realtime-feeds:pusher')),
				is_list (ExtraOptions) ->
	try
		<<"mosaic-examples-realtime-feeds:", TypeSuffix / binary>> = erlang:atom_to_binary (Type, utf8),
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-examples-realtime-feeds-backend--run-", TypeSuffix / binary>>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-examples-realtime-feeds:frontend-java', Identifier, {json, [Configuration]}, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-examples-realtime-feeds-frontend-java--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type = 'mosaic-examples-realtime-feeds:indexer-java', Identifier, {json, [Configuration]}, ExtraOptions)
		when is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-examples-realtime-feeds-indexer-java--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration))]}]}
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
	
configure_1 (Type = 'mosaic-tests:socat', Identifier, {json, [Token, Endpoint]}, ExtraOptions)
			when is_binary (Token), is_binary (Endpoint), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"socat">>)),
		Options = [
				{harness, [
					{argument0, <<"[", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [<<"stdio">>, Endpoint]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_1 (Type, _Identifier, Configuration, _ExtraOptions)
		when is_atom (Type) ->
	{error, {invalid_type_or_configuration, {Type, Configuration}}}.
