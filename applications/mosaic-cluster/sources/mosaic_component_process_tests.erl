
-module (mosaic_component_process_tests).

-export ([test/0]).
-export ([
		test_start_stop/1,
		test_call/1,
		test_cast/1,
		test_abacus/1]).
-export ([configure/5]).

-import (mosaic_process_tests, [start_link_process/4, stop_and_wait_process/1, wait_process/1, call_process/3, cast_process/3]).


-test ({test_start_stop, [{defaults}]}).
-test ({test_call, [{defaults}]}).
-test ({test_cast, [{defaults}]}).
-test ({test_abacus, [{python}]}).
-test ({test_abacus, [{java}]}).


test_start_stop ({defaults}) ->
	{ok, Identifier} = mosaic_cluster:key (),
	{ok, Configuration} = configure (python_parrot, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	ok = stop_and_wait_process (Process),
	ok.


test_call ({defaults}) ->
	{ok, Identifier} = mosaic_cluster:key (),
	{ok, Configuration} = configure (python_parrot, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	Request = {struct, [{<<"key-1">>, 1}, {<<"key-a">>, <<"a">>}]},
	RequestData = <<"data">>,
	{ok, Request, RequestData} = call_process (Process, Request, RequestData),
	ok = stop_and_wait_process (Process),
	ok.


test_cast ({defaults}) ->
	{ok, Identifier} = mosaic_cluster:key (),
	{ok, Configuration} = configure (python_parrot, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	Request = {struct, [{<<"key-1">>, 1}, {<<"key-a">>, <<"a">>}]},
	RequestData = <<"data">>,
	ok = cast_process (Process, Request, RequestData),
	ok = receive {'$gen_cast', {cast, Identifier, Request, RequestData}} -> ok end,
	ok = stop_and_wait_process (Process),
	ok.


test_abacus ({Flavour}) ->
	{ok, Type} = case Flavour of
		python ->
			{ok, python_abacus};
		java ->
			{ok, java_abacus}
	end,
	{ok, Identifier} = mosaic_cluster:key (),
	{ok, Configuration} = configure (Type, Identifier),
	{ok, Process} = start_link_process (mosaic_component_process, create, Identifier, Configuration),
	Request = {struct, [{<<"operator">>, <<"+">>}, {<<"operands">>, [1, 2]}]},
	{ok, {struct, ReplyAttributes}, <<>>} = call_process (Process, Request, <<>>),
	{ok, Outcome} = case lists:sort (ReplyAttributes) of
		[{<<"ok">>, true}, {<<"outcome">>, Outcome_}] when is_number (Outcome_) ->
			{ok, Outcome_}
	end,
	ok = if Outcome == 3 -> ok end,
	ok = stop_and_wait_process (Process),
	ok.


configure (Type, Identifier) ->
	{ok, mosaic_component_process, Configuration} = configure (Type, Identifier, term, defaults, [{router, erlang:self ()}]),
	{ok, Configuration}.


test () ->
	mosaic_tests:test_module (mosaic_component_process_tests).


configure (Type, Identifier, term, defaults, ExtraOptions)
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
			{execute, [
				{executable, Python},
				{argument0, <<"[mosaic_", (erlang:atom_to_binary (Type, utf8)) / binary, "_component#", (mosaic_webmachine:format_string_identifier (Identifier)) / binary, "]">>},
				{arguments, [
					<<"./applications/mosaic-cluster/sources/mosaic_component_harness_tester.py">>,
					<<"simple-backend">>, Scenario, mosaic_webmachine:format_string_identifier (Identifier)]}]}
			| ExtraOptions],
	case mosaic_component_process:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (Type, Identifier, term, defaults, ExtraOptions)
		when (Type =:= java_abacus), is_list (ExtraOptions) ->
	{ok, Java} = case os:find_executable ("java") of
		Java_ when is_list (Java_) ->
			{ok, erlang:list_to_binary (Java_)};
		false ->
			{error, {unresolved_executable, <<"java">>}}
	end,
	Options = [
			{execute, [
				{executable, Java},
				{argument0, <<"[mosaic_", (erlang:atom_to_binary (Type, utf8)) / binary, "_component#", (mosaic_webmachine:format_string_identifier (Identifier)) / binary, "]">>},
				{arguments, [
					<<"-jar">>, <<"../mosaic-java-components/components-container/target/components-container-0.2-SNAPSHOT-jar-with-dependencies.jar">>,
					<<"eu.mosaic_cloud.components.examples.abacus.AbacusComponentCallbacks">>,
					<<"file:../mosaic-java-components/components-examples/target/components-examples-0.2-SNAPSHOT.jar">>]}]}
			| ExtraOptions],
	case mosaic_component_process:parse_configuration (create, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure (_Type, _Identifier, term, Configuration, ExtraOptions)
		when is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}};
	
configure (Type, Identifier, json, null, ExtraOptions)
		when is_list (ExtraOptions) ->
	configure (Type, Identifier, term, defaults, ExtraOptions);
	
configure (Type, Identifier, json, {struct, []}, ExtraOptions)
		when is_list (ExtraOptions) ->
	configure (Type, Identifier, term, defaults, ExtraOptions);
	
configure (_Type, _Identifier, json, Configuration, ExtraOptions)
		when is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.
