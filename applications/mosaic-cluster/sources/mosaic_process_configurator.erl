
-module (mosaic_process_configurator).

-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/0, start_supervised/1]).
-export ([stop/0, stop/1, stop/2]).
-export ([configure/4, configure/5, register/5, register/6, unregister/2, unregister/3, unregister_all/1, unregister_all/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start () ->
	start (defaults).

start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_tools:start (gen_server, mosaic_process_configurator, QualifiedName, Configuration).


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_tools:start_link (gen_server, mosaic_process_configurator, QualifiedName, Configuration).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_sup:start_child_daemon (mosaic_process_configurator, {local, mosaic_process_configurator}, [Configuration], permanent).


stop () ->
	stop (mosaic_process_configurator).

stop (Configurator) ->
	stop (Configurator, normal).

stop (Configurator, Signal)
		when (is_pid (Configurator) orelse is_atom (Configurator)) ->
	gen_server:call (Configurator, {stop, Signal}).


configure (Type, Identifier, ArgumentsEncoding, ArgumentsContent) ->
	configure (mosaic_process_configurator, Type, Identifier, ArgumentsEncoding, ArgumentsContent).

configure (Configurator, Type, Identifier, ArgumentsEncoding, ArgumentsContent)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ArgumentsEncoding) ->
	gen_server:call (Configurator, {configure, Type, Identifier, ArgumentsEncoding, ArgumentsContent}).


register (Type, ArgumentsEncoding, Module, Function, FunctionExtraArguments) ->
	register (mosaic_process_configurator, Type, ArgumentsEncoding, Module, Function, FunctionExtraArguments).

register (Configurator, Type, ArgumentsEncoding, Module, Function, FunctionExtraArguments)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ArgumentsEncoding),
				is_atom (Module), is_atom (Function) ->
	gen_server:call (Configurator, {register, Type, ArgumentsEncoding, Module, Function, FunctionExtraArguments}).


unregister (Type, ArgumentsEncoding) ->
	unregister (mosaic_process_configurator, Type, ArgumentsEncoding).

unregister (Configurator, Type, ArgumentsEncoding)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ArgumentsEncoding) ->
	gen_server:call (Configurator, {unregister, Type, ArgumentsEncoding}).


unregister_all (Type) ->
	unregister_all (mosaic_process_configurator, Type).

unregister_all (Configurator, Type)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type) ->
	gen_server:call (Configurator, {unregister_all, Type}).


-record (state, {qualified_name, delegates}).
-record (delegate, {type, arguments_encoding, function}).


init ({QualifiedName, defaults}) ->
	ok = mosaic_tools:ensure_registered (QualifiedName),
	Delegates = orddict:new (),
	State = #state{qualified_name = QualifiedName, delegates = Delegates},
	Self = erlang:self (),
	_ = spawn (
			fun () ->
				true = erlang:link (Self),
				Function = fun
					(dummy, Identifier, term, defaults) ->
						{ok, mosaic_dummy_process, {identifier, Identifier}};
					(dummy, _Identifier, term, ArgumentsContent) ->
						{error, {invalid_arguments, ArgumentsContent}};
					(dummy, Identifier, json, null) ->
						{ok, mosaic_dummy_process, {identifier, Identifier}};
					(dummy, _Identifier, json, ArgumentsContent) ->
						{error, {invalid_arguments, ArgumentsContent}}
				end,
				ok = gen_server:call (Self, {register, dummy, term, Function}),
				ok = gen_server:call (Self, {register, dummy, json, Function}),
				ok
			end),
	{ok, State}.


terminate (_Reason, _State) ->
	ok.


code_change (_OldVsn, State, _Arguments) ->
	{ok, State}.


handle_call ({configure, Type, Identifier, ArgumentsEncoding, ArgumentsContent}, _Sender, State = #state{delegates = Delegates})
		when is_atom (Type), is_atom (ArgumentsEncoding) ->
	DelegateKey = {Type, ArgumentsEncoding},
	case orddict:find (DelegateKey, Delegates) of
		{ok, #delegate{type = Type, arguments_encoding = ArgumentsEncoding, function = Function}} ->
			try erlang:apply (Function, [Type, Identifier, ArgumentsEncoding, ArgumentsContent]) of
				Outcome = {ok, Module, _Arguments} when is_atom (Module) ->
					{reply, Outcome, State};
				Error = {error, _Reason} ->
					{reply, Error, State};
				Outcome ->
					{reply, {error, {function_failed, {invalid_outcome, Outcome}}}, State}
			catch
				throw : CatchedTerm ->
					{reply, {error, {function_failed, CatchedTerm}}, State};
				error : CatchedTerm ->
					{reply, {error, {function_failed, CatchedTerm}}, State};
				exit : CatchedTerm ->
					{reply, {error, {function_failed, CatchedTerm}}, State}
			end;
		error ->
			{reply, {error, unregistered_function}, State}
	end;
	
handle_call ({register, Type, ArgumentsEncoding, Module, Function, FunctionExtraArguments}, Sender, State)
		when is_atom (Type), is_atom (ArgumentsEncoding), is_atom (Module), is_atom (Function) ->
	FunctionExported = erlang:function_exported (Module, Function, 5),
	if
		FunctionExported ->
			Function_ = fun (Type1, Identifier1, ArgumentsEncoding1, ArgumentsContent1)
					when (Type1 =:= Type), (ArgumentsEncoding1 =:= ArgumentsEncoding) ->
				erlang:apply (Module, Function, [Type1, Identifier1, ArgumentsEncoding1, ArgumentsContent1, FunctionExtraArguments])
			end,
			handle_call ({register, Type, ArgumentsEncoding, Function_}, Sender, State);
		true ->
			{reply, {error, invalid_function}, State}
	end;
	
handle_call ({register, Type, ArgumentsEncoding, Function}, _Sender, OldState = #state{delegates = OldDelegates})
		when is_atom (Type), is_atom (ArgumentsEncoding), is_function (Function) ->
	DelegateKey = {Type, ArgumentsEncoding},
	DelegateRegistered = orddict:is_key (DelegateKey, OldDelegates),
	if
		not DelegateRegistered ->
			FunctionValid = erlang:is_function (Function, 4),
			if
				FunctionValid ->
					Delegate = #delegate{type = Type, arguments_encoding = ArgumentsEncoding, function = Function},
					NewDelegates = orddict:store (DelegateKey, Delegate, OldDelegates),
					NewState = OldState#state{delegates = NewDelegates},
					{reply, ok, NewState};
				true ->
					{reply, {error, invalid_function}, OldState}
			end;
		true ->
			{reply, {error, already_registered_function}, OldState}
	end;
	
handle_call ({unregister, Type, ArgumentsEncoding}, _Sender, OldState = #state{delegates = OldDelegates})
		when is_atom (Type), is_atom (ArgumentsEncoding) ->
	DelegateKey = {Type, ArgumentsEncoding},
	DelegateRegistered = orddict:is_key (DelegateKey, OldDelegates),
	if
		DelegateRegistered ->
			NewDelegates = orddict:erase (DelegateKey, OldDelegates),
			NewState = OldState#state{delegates = NewDelegates},
			{reply, ok, NewState};
		true ->
			{reply, {error, unregistered_function}, OldState}
	end;
	
handle_call ({unregister_all, Type}, _Sender, OldState = #state{delegates = OldDelegates})
		when is_atom (Type) ->
	DelegatesCount = orddict:fold (fun ({Type_, _}, _, Count) when (Type_ =:= Type) -> Count + 1; ({_, _}, _, Count) -> Count end, 0, OldDelegates),
	if
		DelegatesCount > 0 ->
			NewDelegates = orddict:filter (fun ({Type_, _}, _) when (Type_ =:= Type) -> false; ({_, _}, _) -> true end, OldDelegates),
			NewState = OldState#state{delegates = NewDelegates},
			{reply, ok, NewState};
		true ->
			{reply, {error, unregistered_function}, OldState}
	end;
	
handle_call ({stop, Signal}, _Sender, State) ->
	case Signal of
		normal ->
			{stop, normal, ok, State};
		_ ->
			Error = {invalid_signal, Signal},
			{stop, Error, Error, State}
	end;
	
handle_call (Request, _Sender, State) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, State}.


handle_info (Message, State) ->
	Error = {error, {invalid_message, Message}},
	{stop, Error, State}.
