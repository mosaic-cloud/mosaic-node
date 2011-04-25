
-module (mosaic_webmachine).

-export ([start_link/2, enforce_start/0]).
-export ([return_with_content/5, respond_with_content/4]).


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
	{ok, Dispatches} = dispatches ([mosaic_console_wm, mosaic_cluster_wm]),
	Options = [
			{ip, "127.0.0.1"},
			{port, 9999},
			{dispatch, Dispatches},
			{error_handler, webmachine_error_handler},
			{enable_perf_logger, false},
			{log_dir, "/tmp/webmachine"}],
	{ok, _Server} = mosaic_cluster_sup:start_child_daemon (QualifiedName, mosaic_webmachine, [Options], permanent),
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
