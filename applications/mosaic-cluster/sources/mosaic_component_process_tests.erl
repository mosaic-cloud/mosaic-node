
-module (mosaic_component_process_tests).


-export ([test/0]).
-export ([
		test_start_stop/1,
		test_call/1,
		test_cast/1,
		test_migrate/1,
		test_abacus/1,
		test_rabbitmq/1,
		test_riak_kv/1]).
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
%-test ({test_rabbitmq, [{defaults}]}).
%-test ({test_riak_kv, [{defaults}]}).


test_start_stop ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure (python_parrot, create, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	ok = stop_and_wait_process (Process),
	ok.


test_call ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure (python_parrot, create, Identifier),
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
	{ok, Configuration} = configure (python_parrot, create, Identifier),
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
	{ok, SourceConfiguration} = configure (python_parrot, create, Identifier),
	{ok, TargetConfiguration} = configure (python_parrot, migrate, Identifier),
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
			{ok, python_abacus};
		node ->
			{ok, node_abacus};
		java ->
			{ok, java_abacus}
	end,
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure (Type, create, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	{ok, Outcome, <<>>} = call_process (Process, <<"+">>, [1, 2], <<>>),
	ok = if Outcome == 3 -> ok end,
	ok = stop_and_wait_process (Process),
	ok.


test_rabbitmq ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure (rabbitmq, create, Identifier),
	{ok, Router} = mosaic_cluster_processes_router:start_link ({local, mosaic_process_router}, defaults),
	{ok, Resources} = mosaic_cluster_component_resources:start_link ({local, mosaic_component_resources}, defaults),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	ok = timer:sleep (6 * 1000),
	ok = stop_and_wait_process (Process),
	true = erlang:exit (Router, normal),
	true = erlang:exit (Resources, normal),
	ok.


test_riak_kv ({defaults}) ->
	{ok, Identifier} = mosaic_component_coders:generate_component (),
	{ok, Configuration} = configure (riak_kv, create, Identifier),
	{ok, Router} = mosaic_cluster_processes_router:start_link ({local, mosaic_process_router}, defaults),
	{ok, Resources} = mosaic_cluster_component_resources:start_link ({local, mosaic_component_resources}, defaults),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	ok = timer:sleep (6 * 1000),
	ok = stop_and_wait_process (Process),
	true = erlang:exit (Router, normal),
	true = erlang:exit (Resources, normal),
	ok.


configure (Type, create, Identifier) ->
	{ok, mosaic_component_process, Configuration} = configure (Type, create, Identifier, term, defaults, [{router, erlang:self ()}]),
	{ok, Configuration};
	
configure (Type, migrate, Identifier) ->
	{ok, mosaic_component_process, Configuration} = configure (Type, {migrate, target}, Identifier, term, defaults, [{router, erlang:self ()}]),
	{ok, Configuration}.


test () ->
	mosaic_tests:test_module (mosaic_component_process_tests).


configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, defaults) ->
	configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, []);
	
configure (Type, create, Identifier, term, defaults, ExtraOptions)
		when ((Type =:= python_parrot) orelse (Type =:= python_abacus)), is_list (ExtraOptions) ->
	{ok, Python} = case os:find_executable ("python2") of
		Python_ when is_list (Python_) ->
			{ok, erlang:list_to_binary (Python_)};
		false ->
			{error, {unresolved_executable, <<"python2">>}}
	end,
	{ok, Scenario} = case Type of
		python_parrot ->
			{ok, <<"parrot">>};
		python_abacus ->
			{ok, <<"abacus">>}
	end,
	Options = [
			{harness, [
				{argument0, <<"[mosaic_component#", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
			{execute, [
				{executable, Python},
				{arguments, [
					<<"./applications/mosaic-harness/sources/mosaic_harness_tester.py">>,
					<<"backend">>, Scenario, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (Type = node_abacus, create, Identifier, term, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	{ok, Node} = case os:find_executable ("node") of
		Node_ when is_list (Node_) ->
			{ok, erlang:list_to_binary (Node_)};
		false ->
			{error, {unresolved_executable, <<"node">>}}
	end,
	Options = [
			{harness, [
				{argument0, <<"[mosaic_component#", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
			{execute, [
				{executable, Node},
				{arguments, [
					<<"./mosaic_component_abacus.js">>]},
				{working_directory, <<"./applications/mosaic-component/sources">>}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (Type = java_abacus, create, Identifier, term, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	{ok, Java} = case os:find_executable ("java") of
		Java_ when is_list (Java_) ->
			{ok, erlang:list_to_binary (Java_)};
		false ->
			{error, {unresolved_executable, <<"java">>}}
	end,
	Options = [
			{harness, [
				{argument0, <<"[mosaic_component#", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
			{execute, [
				{executable, Java},
				{arguments, [
					<<"-jar">>, <<"../mosaic-java-components/components-container/target/components-container-0.2-SNAPSHOT-jar-with-dependencies.jar">>,
					<<"eu.mosaic_cloud.components.examples.abacus.AbacusComponentCallbacks">>,
					<<"file:../mosaic-java-components/components-examples/target/components-examples-0.2-SNAPSHOT.jar">>]}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (Type = rabbitmq, create, Identifier, term, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	Options = [
			{harness, [
				{argument0, <<"[mosaic_component#", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
			{execute, [
				{executable, <<"./scripts/run-node">>},
				{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]},
				{working_directory, <<"../mosaic-components-rabbitmq">>}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (Type = riak_kv, create, Identifier, term, defaults, ExtraOptions)
		when is_list (ExtraOptions) ->
	Options = [
			{harness, [
				{argument0, <<"[mosaic_component#", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
			{execute, [
				{executable, <<"./scripts/run-node">>},
				{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]},
				{working_directory, <<"../mosaic-components-riak-kv">>}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (_Type, {migrate, source}, _Identifier, term, defaults, _ExtraOptions) ->
	{ok, none, defaults};
	
configure (Type, {migrate, target}, Identifier, term, defaults, ExtraOptions) ->
	Options = [
			{harness, [
				{argument0, <<"[mosaic_component#", (erlang:atom_to_binary (Type, utf8)) / binary, "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]}
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
	
configure (_Type, _Disposition, _Identifier, json, Configuration, ExtraOptions)
		when is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.
