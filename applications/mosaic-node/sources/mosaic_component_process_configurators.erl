
-module (mosaic_component_process_configurators).


-export ([configure/7]).
-export ([configure_hardcoded/6]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


configure (ConfigureCreate, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, defaults) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, []);
	
configure (ConfigureCreate, Type, create, Identifier, term, OriginalConfiguration, ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	case ConfigureCreate (Type, Identifier, OriginalConfiguration, ExtraOptions) of
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
	
configure (ConfigureCreate, Type, {migrate, source}, Identifier, term, defaults, ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	{ok, none, defaults};
	
configure (ConfigureCreate, Type, {migrate, target}, Identifier, term, defaults, ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
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
	
configure (ConfigureCreate, Type, Disposition, Identifier, term, _Configuration, ExtraOptions)
		when is_function (ConfigureCreate, 4), is_atom (Type), is_binary (Identifier), is_list (ExtraOptions) ->
	{error, {invalid_disposition, Disposition}};
	
configure (ConfigureCreate, Type, Disposition, Identifier, json, null, ExtraOptions) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, term, defaults, ExtraOptions);
	
configure (ConfigureCreate, Type, Disposition, Identifier, json, {struct, []}, ExtraOptions) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, term, defaults, ExtraOptions);
	
configure (ConfigureCreate, Type, Disposition, Identifier, json, Configuration, ExtraOptions) ->
	configure (ConfigureCreate, Type, Disposition, Identifier, term, {json, Configuration}, ExtraOptions).



configure_hardcoded (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions) ->
	configure (fun configure_create_hardcoded/4, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent, ExtraOptions).


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
		
		'mosaic-examples-realtime-feeds:fetcher' ->
			configure_create_generic_component (Identifier, <<"mosaic-examples-realtime-feeds-backend--run-fetcher">>, Configuration, ExtraOptions);
		'mosaic-examples-realtime-feeds:indexer' ->
			configure_create_generic_component (Identifier, <<"mosaic-examples-realtime-feeds-backend--run-indexer">>, Configuration, ExtraOptions);
		'mosaic-examples-realtime-feeds:scavanger' ->
			configure_create_generic_component (Identifier, <<"mosaic-examples-realtime-feeds-backend--run-scavanger">>, Configuration, ExtraOptions);
		'mosaic-examples-realtime-feeds:leacher' ->
			configure_create_generic_component (Identifier, <<"mosaic-examples-realtime-feeds-backend--run-leacher">>, Configuration, ExtraOptions);
		'mosaic-examples-realtime-feeds:pusher' ->
			configure_create_generic_component (Identifier, <<"mosaic-examples-realtime-feeds-backend--run-pusher">>, Configuration, ExtraOptions);
		
		'mosaic-examples-realtime-feeds:frontend-java' ->
			configure_create_java_component (Identifier, <<"mosaic-examples-realtime-feeds-frontend-java--run-component">>, Configuration, ExtraOptions);
		'mosaic-examples-realtime-feeds:indexer-java' ->
			configure_create_java_component (Identifier, <<"mosaic-examples-realtime-feeds-indexer-java--run-component">>, Configuration, ExtraOptions);
		
		'mosaic-tests:socat' ->
			configure_create_socat_component (Identifier, Configuration, ExtraOptions);
		'mosaic-tests:exec' ->
			configure_create_exec_component (Identifier, Configuration, ExtraOptions);
		
		_ ->
			{error, {invalid_type, Type}}
	end.


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
