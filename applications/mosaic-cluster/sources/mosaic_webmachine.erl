
-module (mosaic_webmachine).

-export ([start_link/2, enforce_start/0]).
-export ([return_with_outcome/3, respond_with_outcome/3]).
-export ([return_with_content/5, respond_with_content/4]).
-export ([enforce_request/3]).


start_link (QualifiedName = {local, LocalName}, Options)
		when is_atom (LocalName), is_list (Options) ->
	case webmachine_mochiweb:start ([{name, QualifiedName} | Options]) of
		Outcome = {ok, Server} when is_pid (Server) ->
			true = erlang:link (Server),
			Outcome;
		Error = {error, _Reason} ->
			Error
	end.

enforce_start () ->
	QualifiedName = {local, mosaic_webmachine},
	{ok, Dispatches} = dispatches ([mosaic_console_wm, mosaic_cluster_wm, mosaic_executor_wm]),
	ok = case application:get_env (mosaic_cluster, webmachine_listen) of
		undefined ->
			{error, webmachine_unconfigured};
		{ok, {Address, Port}} when is_list (Address), is_number (Port), (Port >= 0), (Port < 65536) ->
			Options = [
					{ip, Address},
					{port, Port},
					{dispatch, Dispatches},
					{error_handler, webmachine_error_handler},
					{enable_perf_logger, false},
					{log_dir, undefined}],
			case mosaic_cluster_sup:start_child_daemon (QualifiedName, mosaic_webmachine, [Options], permanent) of
				{ok, Server} when is_pid (Server) ->
					true = erlang:unlink (Server),
					ok;
				Error = {error, _Reason} ->
					Error
			end
	end,
	ok.


dispatches (Modules)
		when is_list (Modules) ->
	case dispatches (Modules, []) of
		{ok, Dispatches} ->
			{ok, lists:reverse (Dispatches)};
		Error = {error, _Reason} ->
			Error
	end;
	
dispatches (Module)
		when is_atom (Module) ->
	Dispatches = lists:flatten (
			lists:map (
				fun
					({dispatch, [{Path, Arguments}]})
							when is_list (Path) ->
						[{Path, Module, Arguments}];
					({Name, _Value})
							when (Name =/= dispatch) ->
						[]
				end,
				erlang:apply (Module, module_info, [attributes]))),
	{ok, Dispatches}.


dispatches ([], Accumulator) ->
	{ok, Accumulator};
	
dispatches ([Module | Modules], Accumulator) ->
	Outcome = case dispatches (Module) of
		Outcome_ = {ok, []} ->
			Outcome_;
		Outcome_ = {ok, [_]} ->
			Outcome_;
		{ok, Dispatches_ = [_|_]} ->
			{ok, lists:reverse (Dispatches_)}
	end,
	case Outcome of
		{ok, Dispatches} ->
			dispatches (Modules, Dispatches ++ Accumulator);
		Error = {error, _Reason} ->
			Error
	end.


return_with_outcome (Outcome, Request, State) ->
	case Outcome of
		{ok, Return} ->
			{Return, Request, State};
		{ok, Return, NewState} ->
			{Return, Request, NewState};
		{error, Reason} ->
			mosaic_webmachine:return_with_content (true, error, Reason, Request, State)
	end.

respond_with_outcome (Outcome, Request, State) ->
	case Outcome of
		ok ->
			mosaic_webmachine:respond_with_content (json, {struct, [{ok, true}]}, Request, State);
		{ok, json_struct, AttributeTerms} ->
			mosaic_webmachine:respond_with_content (json, {struct, [{ok, true} | AttributeTerms]}, Request, State);
		{error, Reason} ->
			mosaic_webmachine:respond_with_content (error, Reason, Request, State)
	end.


return_with_content (Return, Type, ContentTerm, Request, State) ->
	{ok, ContentType, Content} = encode_content (Type, ContentTerm),
	Response = wrq:set_resp_body (Content, wrq:set_resp_header ("Content-Type", ContentType, Request)),
	{Return, Response, State}.

respond_with_content (Type, ContentTerm, Request, State) ->
	{ok, ContentType, Content} = encode_content (Type, ContentTerm),
	Response = wrq:set_resp_header ("Content-Type", ContentType, Request),
	{Content, Response, State}.


encode_content (json, ContentTerm) ->
	{ok, "application/json", mochijson2:encode (ContentTerm)};
	
encode_content (error, ReasonTerm) ->
	ReasonString = erlang:iolist_to_binary (io_lib:format ("~76p", [ReasonTerm])),
	encode_content (json, {struct, [{ok, false}, {error, ReasonString}]}).


enforce_request (Method, ArgumentNames, Request)
		when is_atom (Method), is_list (ArgumentNames) ->
	case wrq:method (Request) of
		Method ->
			case lists:sort (lists:map (fun ({Name, _Value}) -> Name end, wrq:req_qs (Request))) of
				ArgumentNames ->
					case ArgumentNames of
						[] ->
							{ok, false};
						_ ->
							ArgumentValues = lists:map (fun (Name) -> wrq:get_qs_value (Name, Request) end, ArgumentNames),
							{ok, false, ArgumentValues}
						end;
				OtherArgumentNames ->
					{error, {invalid_query, {invalid_arguments, OtherArgumentNames}}}
			end;
		OtherMethod ->
			{error, {invalid_method, OtherMethod}}
	end.
