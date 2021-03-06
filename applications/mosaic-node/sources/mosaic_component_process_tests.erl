
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


configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions) ->
	mosaic_component_process_configurators:configure (fun configure_create/4, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions).


configure_create (Type, Identifier, Configuration, ExtraOptions)
		when is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	{error, not_implemented}.
