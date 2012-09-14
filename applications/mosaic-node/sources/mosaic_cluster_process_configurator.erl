
-module (mosaic_cluster_process_configurator).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


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
	{ok, FunctionKey} = mosaic_cluster_tools:key ({mosaic_cluster_processes, configurator, Type, ConfigurationEncoding}),
	case mosaic_cluster_storage:select (FunctionKey) of
		{ok, undefined, {mosaic_cluster_processes, configurator, Type, ConfigurationEncoding, Function, _Annotation}} when is_function (Function, 5) ->
			try erlang:apply (Function, [Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent]) of
				Outcome = {ok, Module, _Configuration} when is_atom (Module) ->
					{reply, Outcome, State};
				Error = {error, _Reason} ->
					{reply, Error, State};
				Return ->
					{reply, {error, {configurator_failed, {invalid_return, Return}}}, State}
			catch
				throw : CatchedTerm ->
					{reply, {error, {configurator_failed, {throw, CatchedTerm, erlang:get_stacktrace ()}}}, State};
				error : CatchedTerm ->
					{reply, {error, {configurator_failed, {error, CatchedTerm, erlang:get_stacktrace ()}}}, State};
				exit : CatchedTerm ->
					{reply, {error, {configurator_failed, {exit, CatchedTerm, erlang:get_stacktrace ()}}}, State}
			end;
		{ok, _, _} ->
			ok = mosaic_transcript:trace_error ("selected invalid configurator; ignoring!", [{type, Type}, {configuration_encoding, ConfigurationEncoding}]),
			{reply, {error, configurator_not_registered}, State};
		{error, does_not_exist} ->
			{reply, {error, configurator_not_registered}, State}
	end;
	
handle_call ({mosaic_process_configurator, register, Type, ConfigurationEncoding, {Module, Function, FunctionExtraArgument}, Annotation}, Sender, State = #state{})
		when is_atom (Type), is_atom (ConfigurationEncoding), is_atom (Module), is_atom (Function) ->
	_ = code:ensure_loaded (Module),
	ModuleLoaded = erlang:module_loaded (Module),
	FunctionExported = erlang:function_exported (Module, Function, 6),
	if
		ModuleLoaded, FunctionExported ->
			Function_ = fun (Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_) ->
				erlang:apply (Module, Function, [Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_, FunctionExtraArgument])
			end,
			handle_call ({mosaic_process_configurator, register, Type, ConfigurationEncoding, Function_, Annotation}, Sender, State);
		not ModuleLoaded ->
			{reply, {error, {invalid_module, Module}}, State};
		not FunctionExported ->
			{reply, {error, {invalid_function, {Module, Function}}}, State}
	end;
	
handle_call ({mosaic_process_configurator, register, Type, ConfigurationEncoding, {Module, Function}, Annotation}, Sender, State = #state{})
		when is_atom (Type), is_atom (ConfigurationEncoding), is_atom (Module), is_atom (Function) ->
	_ = code:ensure_loaded (Module),
	ModuleLoaded = erlang:module_loaded (Module),
	FunctionExported = erlang:function_exported (Module, Function, 5),
	if
		ModuleLoaded, FunctionExported ->
			Function_ = fun (Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_) ->
				erlang:apply (Module, Function, [Type_, Disposition_, Identifier_, ConfigurationEncoding_, ConfigurationContent_])
			end,
			handle_call ({mosaic_process_configurator, register, Type, ConfigurationEncoding, Function_, Annotation}, Sender, State);
		not ModuleLoaded ->
			{reply, {error, {invalid_module, Module}}, State};
		not FunctionExported ->
			{reply, {error, {invalid_function, {Module, Function}}}, State}
	end;
	
handle_call ({mosaic_process_configurator, register, Type, ConfigurationEncoding, Function, Annotation}, _Sender, State = #state{})
		when is_atom (Type), is_atom (ConfigurationEncoding), is_function (Function) ->
	FunctionValid = erlang:is_function (Function, 5),
	if
		FunctionValid ->
			{ok, FunctionKey} = mosaic_cluster_tools:key ({mosaic_cluster_processes, configurator, Type, ConfigurationEncoding}),
			case mosaic_cluster_storage:include (FunctionKey, undefined, {mosaic_cluster_processes, configurator, Type, ConfigurationEncoding, Function, Annotation}) of
				ok ->
					{reply, ok, State};
				Error = {error, _Reason} ->
					{reply, Error, State}
			end;
		true ->
			{reply, {error, {invalid_function, Function}}, State}
	end;
	
handle_call ({mosaic_process_configurator, unregister, Type, ConfigurationEncoding}, _Sender, State = #state{})
		when is_atom (Type), is_atom (ConfigurationEncoding) ->
	{ok, FunctionKey} = mosaic_cluster_tools:key ({mosaic_cluster_processes, configurator, Type, ConfigurationEncoding}),
	case mosaic_cluster_storage:exculde (FunctionKey, undefined) of
		ok ->
			{reply, ok, State};
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_call ({mosaic_process_configurator, select}, _Sender, State = #state{}) ->
	case mosaic_cluster_storage:map (
			fun
				(_Key, {undefined, {mosaic_cluster_processes, configurator, Type, ConfigurationEncoding, _Function, Annotation}})
						when is_atom (Type), is_atom (ConfigurationEncoding) ->
					{ok, {Type, ConfigurationEncoding, Annotation}};
				(_, _) ->
					ok
			end)
	of
		{ok, Configurators, []} ->
			{reply, {ok, lists:usort (Configurators)}, State};
		{ok, _, Reasons} ->
			{reply, {error, {storage_failure, Reasons}}, State}
	end;
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, State = #state{}) ->
	{stop, {error, {invalid_message, Message}}, State}.
