
-module (mosaic_cluster_process_configurator).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1, enforce_ok_2/1]).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_node_sup:start_child_daemon ({local, mosaic_process_configurator}, mosaic_cluster_process_configurator, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_cluster_process_configurator, QualifiedName, Configuration).


-record (state, {qualified_name}).


init ({QualifiedName, defaults}) ->
	case mosaic_process_tools:ensure_registered (QualifiedName) of
		ok ->
			State = #state{qualified_name = QualifiedName},
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{}) ->
	ok.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_process_configurator, configure, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent}, _Sender, State = #state{})
		when is_atom (Type), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_atom (ConfigurationEncoding),
				((Disposition =:= create) orelse (is_record (Disposition, migrate, 2) andalso ((element (2, Disposition) =:= source) orelse (element (2, Disposition) =:= target)))) ->
	try
		{Module, Configuration} = enforce_ok_2 (execute_configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent)),
		{reply, {ok, Module, Configuration}, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_configurator, register, Type, ConfigurationEncoding, OriginalFunction, Annotation}, _Sender, State = #state{})
		when is_atom (Type), is_atom (ConfigurationEncoding) ->
	try
		CoercedFunction = enforce_ok_1 (coerce_configurator_function (OriginalFunction)),
		enforce_ok (execute_register (Type, ConfigurationEncoding, CoercedFunction, Annotation)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_configurator, unregister, Type, ConfigurationEncoding}, _Sender, State = #state{})
		when is_atom (Type), is_atom (ConfigurationEncoding) ->
	try
		enforce_ok (execute_unregister (Type, ConfigurationEncoding)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call ({mosaic_process_configurator, select}, _Sender, State = #state{}) ->
	try
		Informations = enforce_ok_1 (execute_select ()),
		{reply, {ok, Informations}, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, State = #state{}) ->
	{stop, {error, {invalid_message, Message}}, State}.


execute_configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, configurator, Type, ConfigurationEncoding})),
	case mosaic_cluster_storage:select (Key) of
		{ok, undefined, {mosaic_cluster_processes, configurator, Type, ConfigurationEncoding, Function, _Annotation}} when is_function (Function, 5) ->
			try erlang:apply (Function, [Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent]) of
				Outcome = {ok, Module, _Configuration} when is_atom (Module) ->
					Outcome;
				Error = {error, _Reason} ->
					Error;
				Return ->
					{error, {configurator_failed, {invalid_return, Return}}}
			catch
				throw : CatchedTerm ->
					{error, {configurator_failed, {throw, CatchedTerm, erlang:get_stacktrace ()}}};
				error : CatchedTerm ->
					{error, {configurator_failed, {error, CatchedTerm, erlang:get_stacktrace ()}}};
				exit : CatchedTerm ->
					{error, {configurator_failed, {exit, CatchedTerm, erlang:get_stacktrace ()}}}
			end;
		{ok, _, _} ->
			ok = mosaic_transcript:trace_error ("selected invalid configurator; ignoring!", [{type, Type}, {configuration_encoding, ConfigurationEncoding}]),
			{error, configurator_not_registered};
		{error, does_not_exist} ->
			{error, configurator_not_registered};
		Error = {error, _Reason} ->
			Error
	end.


execute_register (Type, ConfigurationEncoding, Function, Annotation) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, configurator, Type, ConfigurationEncoding})),
	enforce_ok (mosaic_cluster_storage:include (Key, undefined, {mosaic_cluster_processes, configurator, Type, ConfigurationEncoding, Function, Annotation})),
	ok.


execute_unregister (Type, ConfigurationEncoding) ->
	Key = enforce_ok_1 (mosaic_cluster_tools:key ({mosaic_cluster_processes, configurator, Type, ConfigurationEncoding})),
	enforce_ok (mosaic_cluster_storage:exculde (Key, undefined)),
	ok.


execute_select () ->
	case mosaic_cluster_storage:map (
			fun
				(Key, {undefined, {mosaic_cluster_processes, configurator, Type, ConfigurationEncoding, Function, Annotation}})
						when is_atom (Type), is_atom (ConfigurationEncoding) ->
					Information = orddict:from_list ([
							{key, Key},
							{type, Type},
							{configuration_encoding, ConfigurationEncoding},
							{function, Function},
							{annotation, Annotation}]),
					{ok, Information};
				(_, _) ->
					ok
			end)
	of
		{ok, Informations, []} ->
			Comparator = fun (Information_1, Information_2) ->
				{orddict:fetch (type, Information_1), orddict:fetch (configuration_encoding, Information_1)}
						=< {orddict:fetch (type, Information_2), orddict:fetch (configuration_encoding, Information_2)}
			end,
			{ok, lists:usort (Comparator, Informations)};
		{ok, _, Reasons} ->
			{error, {storage_failure, Reasons}}
	end.


coerce_configurator_function ({Module, Function, FunctionExtraArgument})
		when is_atom (Module), is_atom (Function) ->
	_ = code:ensure_loaded (Module),
	ModuleLoaded = erlang:module_loaded (Module),
	FunctionExported = erlang:function_exported (Module, Function, 6),
	if
		ModuleLoaded, FunctionExported ->
			Function_ = fun (Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_) ->
				erlang:apply (Module, Function, [Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_, FunctionExtraArgument])
			end,
			{ok, Function_};
		not ModuleLoaded ->
			{error, {invalid_module, Module}};
		not FunctionExported ->
			{error, {invalid_function, {Module, Function}}}
	end;
	
coerce_configurator_function ({Module, Function})
		when is_atom (Module), is_atom (Function) ->
	_ = code:ensure_loaded (Module),
	ModuleLoaded = erlang:module_loaded (Module),
	FunctionExported = erlang:function_exported (Module, Function, 5),
	if
		ModuleLoaded, FunctionExported ->
			Function_ = fun (Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_) ->
				erlang:apply (Module, Function, [Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_])
			end,
			{ok, Function_};
		not ModuleLoaded ->
			{error, {invalid_module, Module}};
		not FunctionExported ->
			{error, {invalid_function, {Module, Function}}}
	end;
	
coerce_configurator_function (Function)
		when is_function (Function, 5) ->
	{ok, Function};
	
coerce_configurator_function (Function) ->
	{error, {invalid_function, Function}}.
