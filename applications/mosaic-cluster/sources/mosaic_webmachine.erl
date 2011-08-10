
-module (mosaic_webmachine).

-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/0, start_supervised/1]).
-export ([return_with_outcome/3, respond_with_outcome/3]).
-export ([return_with_content/5, respond_with_content/4]).
-export ([enforce_request/3]).


start () ->
	start (defaults).

start (Configuration) ->
	start ({local, mosaic_webmachine}, Configuration).

start (QualifiedName = {local, LocalName}, Configuration)
		when is_atom (LocalName) ->
	case options (Configuration) of
		{ok, Options} ->
			case webmachine_mochiweb:start ([{name, QualifiedName} | Options]) of
				Outcome = {ok, _Webmachine} ->
					Outcome;
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	start_link ({local, mosaic_webmachine}, Configuration).

start_link (QualifiedName, Configuration) ->
	case start (QualifiedName, Configuration) of
		Outcome = {ok, Webmachine} ->
			true = erlang:link (Webmachine),
			Outcome;
		Error = {error, _Reason} ->
			Error
	end.


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_cluster_sup:start_child_daemon (mosaic_webmachine, {local, mosaic_webmachine}, [Configuration], permanent).


options (defaults) ->
	case application:get_env (mosaic_cluster, webmachine_listen) of
		{ok, {Address, Port}} when is_list (Address), is_integer (Port), (Port >= 0), (Port < 65536) ->
			case dispatches ([mosaic_cluster_wm, mosaic_cluster_processes_wm]) of
				{ok, Dispatches} ->
					Options = [
							{ip, Address},
							{port, Port},
							{dispatch, Dispatches},
							{error_handler, webmachine_error_handler},
							{enable_perf_logger, false},
							{log_dir, undefined}],
					{ok, Options};
				Error = {error, _Reason} ->
					Error
			end;
		undefined ->
			{error, unconfigured}
	end;
	
options (Configuration) ->
	{error, {invalid_configuration, Configuration}}.


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
		{ok, html, Body} ->
			mosaic_webmachine:respond_with_content (html, Body, Request, State);
		{ok, json_struct, AttributeTerms} ->
			mosaic_webmachine:respond_with_content (json, {struct, [{ok, true} | AttributeTerms]}, Request, State);
		{ok, {mime, MimeType}, Body} when is_binary (MimeType) ->
			mosaic_webmachine:respond_with_content ({mime, MimeType}, Body, Request, State);
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


encode_content (html, Content) ->
	{ok, "text/html", Content};
	
encode_content (json, Content) ->
	case mosaic_json_coders:encode_json (Content) of
		{ok, EncodedContent} ->
			{ok, "application/json", EncodedContent};
		Error = {error, _Reason} ->
			Error
	end;
	
encode_content ({mime, MimeType}, Content)
		when is_binary (MimeType) ->
	{ok, erlang:binary_to_list (MimeType), Content};
	
encode_content (error, Reason) ->
	case mosaic_generic_coders:encode_reason (json, Reason) of
		{ok, EncodedReason} ->
			encode_content (json, {struct, [{ok, false}, {error, EncodedReason}]});
		Error = {error, _Reason} ->
			Error
	end.


enforce_request (Method, Arguments, Request)
		when is_atom (Method), is_list (Arguments) ->
	case wrq:method (Request) of
		Method ->
			case Arguments of
				[] ->
					{ok, false};
				_ ->
					case parse_arguments (Arguments, Request) of
						{ok, ArgumentNames, ArgumentValues} ->
							case lists:filter (
									fun (Name) -> not lists:member (Name, ArgumentNames) end,
									lists:map (fun ({Name, _}) -> Name end, wrq:req_qs (Request))) of
								[] ->
									{ok, false, ArgumentValues};
								UnexpectedArgumentNames ->
									{error, {unexpected_arguments, UnexpectedArgumentNames}}
							end;
						{error, Reason} ->
							{error, Reason}
					end
			end;
		OtherMethod ->
			{error, {invalid_method, OtherMethod}}
	end.


parse_arguments (Arguments, Request)
		when is_list (Arguments) ->
	case parse_arguments (Arguments, Request, [], []) of
		{ok, Names, Values} ->
			{ok, lists:reverse (Names), lists:reverse (Values)};
		Error = {error, _Reason} ->
			Error
	end.

parse_arguments ([], _Request, Names, Values) ->
	{ok, Names, Values};
	
parse_arguments ([Name | Arguments], Request, Names, Values)
		when is_list (Name) ->
	case wrq:get_qs_value (Name, Request) of
		Value when is_list (Value) ->
			parse_arguments (Arguments, Request, [Name | Names], [Value | Values]);
		undefined ->
			{error, {missing_argument, Name}}
	end;
	
parse_arguments ([{Name, Parser} | Arguments], Request, Names, Values)
		when is_list (Name), is_function (Parser, 1) ->
	case wrq:get_qs_value (Name, Request) of
		ValueString when is_list (ValueString) ->
			case Parser (ValueString) of
				{ok, ValueTerm} ->
					parse_arguments (Arguments, Request, [Name | Names], [ValueTerm | Values]);
				{error, Reason} ->
					{error, {invalid_argument, Name, Reason}}
			end;
		undefined ->
			{error, {missing_argument, Name}}
	end.
