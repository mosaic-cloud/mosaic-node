
-module (mosaic_component_process_configurators).


-export ([configure/7]).
-export ([configure_hardcoded/6]).
-export ([configure_static/6]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


configure (ConfigureCreate, Type, create, Identifier, term, OriginalConfiguration, ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier) ->
	case ConfigureCreate (Type, Identifier, OriginalConfiguration, ExtraOptions) of
		{ok, Options} when is_list (Options) ->
			case mosaic_component_process_coders:parse_configuration (create, term, Options) of
				{ok, Configuration} ->
					{ok, mosaic_component_process, Configuration};
				Error = {error, _Reason} ->
					Error
			end;
		Outcome = {ok, Module, _Configuration} when is_atom (Module) ->
			Outcome;
		Error = {error, _Reason} ->
			Error
	end;
	
configure (ConfigureCreate, Type, {migrate, Disposition}, Identifier, term, Configuration, ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), ((Disposition =:= source) orelse (Disposition =:= target)), is_binary (Identifier) ->
	configure_migrate (Disposition, Type, Identifier, Configuration, ExtraOptions);
	
configure (ConfigureCreate, Type, Disposition, Identifier, term, _Configuration, _ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier) ->
	{error, {invalid_disposition, Disposition}};
	
configure (ConfigureCreate, Type, Disposition, Identifier, json, null, ExtraOptions) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, term, defaults, ExtraOptions);
	
configure (ConfigureCreate, Type, Disposition, Identifier, json, {struct, []}, ExtraOptions) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, term, defaults, ExtraOptions);
	
configure (ConfigureCreate, Type, Disposition, Identifier, json, Configuration, ExtraOptions) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, term, {json, Configuration}, ExtraOptions);
	
configure (ConfigureCreate, Type, _Disposition, Identifier, ConfigurationEncoding, _Configuration, _ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier) ->
	{error, {invalid_configuration_encoding, ConfigurationEncoding}}.


configure_migrate (Disposition, Type, Identifier, Configuration, defaults) ->
	configure_migrate (Disposition, Type, Identifier, Configuration, []);
	
configure_migrate (source, Type, Identifier, defaults, ExtraOptions)
		when is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	{ok, none, defaults};
	
configure_migrate (target, Type, Identifier, defaults, ExtraOptions)
		when is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	Options = [
			{harness, [
				{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]}
			| ExtraOptions],
	case mosaic_component_process_coders:parse_configuration (migrate, term, Options) of
		{ok, Configuration} ->
			{ok, mosaic_component_process, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
configure_migrate (Disposition, Type, Identifier, defaults, ExtraOptions)
		when ((Disposition =:= source) orelse (Disposition =:= target)), is_atom (Type), is_binary (Identifier) ->
	{error, {invalid_extra_options, ExtraOptions}};
	
configure_migrate (Disposition, Type, Identifier, Configuration, _ExtraOptions)
		when ((Disposition =:= source) orelse (Disposition =:= target)), is_atom (Type), is_binary (Identifier) ->
	{error, {invalid_configuration, Configuration}}.


configure_hardcoded (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions) ->
	configure (fun configure_create_hardcoded/4, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions).


configure_create_hardcoded (Type, Identifier, Configuration, defaults) ->
	configure_create_hardcoded (Type, Identifier, Configuration, []);
	
configure_create_hardcoded (Type, Identifier, Configuration, ExtraOptions)
		when is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	
	case Type of
		
		'mosaic-components:rabbitmq' ->
			configure_create_generic_component (Identifier, <<"mosaic-components-rabbitmq--run-component">>, Configuration, ExtraOptions);
		'mosaic-components:riak-kv' ->
			configure_create_generic_component (Identifier, <<"mosaic-components-riak-kv--run-component">>, Configuration, ExtraOptions);
		'mosaic-components:couchdb' ->
			configure_create_generic_component (Identifier, <<"mosaic-components-couchdb--run-component">>, Configuration, ExtraOptions);
		'mosaic-components:httpg' ->
			configure_create_generic_component (Identifier, <<"mosaic-components-httpg--run-component">>, Configuration, ExtraOptions);
		
		'mosaic-components:java-component-container' ->
			configure_create_java_component_container (Identifier, Configuration, ExtraOptions);
		
		'mosaic-components:java-cloudlet-container' ->
			configure_create_java_cloudlet_container (Identifier, Configuration, ExtraOptions);
		
		'mosaic-components:java-driver-amqp' ->
			configure_create_java_component (Identifier, <<"mosaic-components-java-driver-amqp--run-component">>, Configuration, ExtraOptions);
		'mosaic-components:java-driver-riak' ->
			configure_create_java_component (Identifier, <<"mosaic-components-java-driver-riak--run-component">>, Configuration, ExtraOptions);
		'mosaic-components:java-driver-hdfs' ->
			configure_create_java_component (Identifier, <<"mosaic-components-java-driver-hdfs--run-component">>, Configuration, ExtraOptions);
		
		'mosaic-applications-realtime-feeds:fetcher' ->
			configure_create_generic_component (Identifier, <<"mosaic-applications-realtime-feeds-backend--run-fetcher">>, Configuration, ExtraOptions);
		'mosaic-applications-realtime-feeds:indexer' ->
			configure_create_generic_component (Identifier, <<"mosaic-applications-realtime-feeds-backend--run-indexer">>, Configuration, ExtraOptions);
		'mosaic-applications-realtime-feeds:scavanger' ->
			configure_create_generic_component (Identifier, <<"mosaic-applications-realtime-feeds-backend--run-scavanger">>, Configuration, ExtraOptions);
		'mosaic-applications-realtime-feeds:leacher' ->
			configure_create_generic_component (Identifier, <<"mosaic-applications-realtime-feeds-backend--run-leacher">>, Configuration, ExtraOptions);
		'mosaic-applications-realtime-feeds:pusher' ->
			configure_create_generic_component (Identifier, <<"mosaic-applications-realtime-feeds-backend--run-pusher">>, Configuration, ExtraOptions);
		
		'mosaic-applications-realtime-feeds:frontend-java' ->
			configure_create_java_component (Identifier, <<"mosaic-applications-realtime-feeds-frontend-java--run-component">>, Configuration, ExtraOptions);
		'mosaic-applications-realtime-feeds:indexer-java' ->
			configure_create_java_component (Identifier, <<"mosaic-applications-realtime-feeds-indexer-java--run-component">>, Configuration, ExtraOptions);
		
		'mosaic-tests:socat' ->
			configure_create_socat_component (Identifier, Configuration, ExtraOptions);
		'mosaic-tests:exec' ->
			configure_create_exec_component (Identifier, Configuration, ExtraOptions);
		
		_ ->
			{error, {invalid_type, Type}}
	end;
	
configure_create_hardcoded (Type, Identifier, _Configuration, ExtraOptions)
		when is_atom (Type), is_binary (Identifier) ->
	{error, {invalid_extra_options, ExtraOptions}}.


configure_create_generic_component (Identifier, ExecutableName, defaults, ExtraOptions)
		when is_binary (Identifier), is_binary (ExecutableName), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (ExecutableName)),
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_generic_component (Identifier, ExecutableName, Configuration, ExtraOptions)
		when is_binary (Identifier), is_binary (ExecutableName), is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.


configure_create_java_component_container (Identifier, {json, [Class, ClassPath]}, ExtraOptions)
		when is_binary (Identifier), is_binary (Class), is_binary (ClassPath), is_list (ExtraOptions) ->
	configure_create_java_component_container (Identifier, {json, [Class, null, [ClassPath]]}, ExtraOptions);
	
configure_create_java_component_container (Identifier, {json, [Class, Configuration, ClassPath]}, ExtraOptions)
		when is_binary (Identifier), is_binary (Class), is_list (ClassPath), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-component-container--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
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
	
configure_create_java_component_container (Identifier, {json, [Class, Configuration, ClassPath, Logger]}, ExtraOptions)
		when is_binary (Identifier), is_binary (Class), is_list (ClassPath), is_binary (Logger), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-component-container--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-class">>, Class,
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration)),
						<<"--component-classpath">>, erlang:list_to_binary (string:join (lists:map (fun erlang:binary_to_list/1, ClassPath), "|")),
						<<"--component-logging-endpoint">>, Logger]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_java_component_container (Identifier, Configuration, ExtraOptions)
		when is_binary (Identifier), is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.


configure_create_java_cloudlet_container (Identifier, {json, [Descriptor, ClassPath]}, ExtraOptions)
		when is_binary (Identifier), is_binary (Descriptor), is_binary (ClassPath), is_list (ExtraOptions) ->
	configure_create_java_cloudlet_container (Identifier, {json, [{struct, [{<<"descriptor">>, Descriptor}]}, [ClassPath]]}, ExtraOptions);
	
configure_create_java_cloudlet_container (Identifier, {json, [Configuration, ClassPath]}, ExtraOptions)
		when is_binary (Identifier), is_list (ClassPath), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"mosaic-components-java-cloudlet-container--run-component">>)),
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration)),
						<<"--component-classpath">>, erlang:list_to_binary (string:join (lists:map (fun erlang:binary_to_list/1, ClassPath), "|"))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_java_cloudlet_container (Identifier, Configuration, ExtraOptions)
		when is_binary (Identifier), is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.


configure_create_java_component (Identifier, ExecutableName, defaults, ExtraOptions)
		when is_binary (Identifier), is_binary (ExecutableName), is_list (ExtraOptions) ->
	configure_create_java_component (Identifier, ExecutableName, {json, [null]}, ExtraOptions);
	
configure_create_java_component (Identifier, ExecutableName, {json, [Configuration]}, ExtraOptions)
		when is_binary (Identifier), is_binary (ExecutableName), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (ExecutableName)),
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [
						<<"--component-identifier">>, enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
						<<"--component-callbacks-configuration">>, enforce_ok_1 (mosaic_json_coders:encode_json (Configuration))]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_java_component (Identifier, ExecutableName, Configuration, ExtraOptions)
		when is_binary (Identifier), is_binary (ExecutableName), is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.


configure_create_socat_component (Identifier, {json, Endpoint}, ExtraOptions)
		when is_binary (Identifier), is_binary (Endpoint), is_list (ExtraOptions) ->
	try
		Executable = enforce_ok_1 (mosaic_generic_coders:os_bin_get (<<"socat">>)),
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, [<<"stdio">>, Endpoint]}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_socat_component (Identifier, {json, [Token, Endpoint]}, ExtraOptions)
		when is_binary (Identifier), is_binary (Token), is_binary (Endpoint), is_list (ExtraOptions) ->
	configure_create_socat_component (Identifier, {json, Endpoint}, ExtraOptions);
	
configure_create_socat_component (Identifier, Configuration, ExtraOptions)
		when is_binary (Identifier), is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.


configure_create_exec_component (Identifier, {json, [Executable, Arguments]}, ExtraOptions)
		when is_binary (Identifier), is_list (ExtraOptions) ->
	try
		Options = [
				{harness, [
					{argument0, <<"[", "#", (enforce_ok_1 (mosaic_component_coders:encode_component (Identifier))) / binary, "]">>}]},
				{execute, [
					{executable, Executable},
					{arguments, Arguments}]}
				| ExtraOptions],
		{ok, Options}
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_exec_component (Identifier, Configuration, ExtraOptions)
		when is_binary (Identifier), is_list (ExtraOptions) ->
	{error, {invalid_configuration, Configuration}}.


configure_static (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions) ->
	configure (fun configure_create_static/4, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions).


configure_create_static (Type, Identifier, Configuration, defaults) ->
	configure_create_static (Type, Identifier, Configuration, []);
	
configure_create_static (Type, Identifier, Configuration, {json, {struct, Context}}) ->
	configure_create_static (Type, Identifier, Configuration, Context);
	
configure_create_static (_, Identifier, defaults, Context)
		when is_list (Context) ->
	try
		case lists:sort (Context) of
			[{<<"configuration">>, Configuration}, {<<"type">>, <<$#, Type_ / binary>>}] ->
				Type = enforce_ok_1 (mosaic_generic_coders:decode_atom (Type_)),
				configure_hardcoded (Type, create, Identifier, json, Configuration, defaults);
			_ ->
				{error, {invalid_context, Context}}
		end
	catch throw : Error = {error, _Reason} -> Error end;
	
configure_create_static (_Type, _Identifier, defaults, Context) ->
	{error, {invalid_context, Context}};
	
configure_create_static (_Type, _Identifier, Configuration, _Context) ->
	{error, {invalid_configuration, Configuration}}.
