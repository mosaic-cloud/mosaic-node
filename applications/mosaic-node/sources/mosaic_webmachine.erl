
-module (mosaic_webmachine).

-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/0, start_supervised/1]).
-export ([return_with_outcome/3, respond_with_outcome/3]).
-export ([return_with_content/5, respond_with_content/4]).
-export ([enforce_get_request/2, enforce_post_request/3]).


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
	mosaic_node_sup:start_child_daemon (mosaic_webmachine, {local, mosaic_webmachine}, [Configuration], permanent).


options (defaults) ->
	case application:get_env (mosaic_node, webmachine_listen) of
		{ok, {Address, Port}} when is_list (Address), is_integer (Port), (Port >= 0), (Port < 65536) ->
			case dispatches ([mosaic_cluster_wm, mosaic_cluster_processes_wm, mosaic_static_resources_wm]) of
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
						[{lists:map (fun (Token) when is_binary (Token) -> erlang:binary_to_list (Token); (Token) when is_atom (Token) -> Token end, Path), Module, Arguments}];
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
		{ok, Return, html, Body} when is_binary (Body) ->
			return_with_content (Return, html, Body, Request, State);
		{ok, Return, json, Object} ->
			return_with_content (Return, json, Object, Request, State);
		{ok, Return, json_struct, Attributes} ->
			return_with_content (Return, json, {struct, [{ok, true} | Attributes]}, Request, State);
		{ok, Return, {mime, MimeType}, Body} when is_binary (MimeType) ->
			return_with_content (Return, {mime, MimeType}, Body, Request, State);
		{error, Return, Reason} ->
			return_with_content (Return, error, Reason, Request, State)
	end.

respond_with_outcome (Outcome, Request, State) ->
	case Outcome of
		ok ->
			respond_with_content (json, {struct, [{ok, true}]}, Request, State);
		{ok, html, Body} when is_binary (Body) ->
			respond_with_content (html, Body, Request, State);
		{ok, json, Object} ->
			respond_with_content (json, Object, Request, State);
		{ok, json_struct, Attributes} ->
			respond_with_content (json, {struct, [{ok, true} | Attributes]}, Request, State);
		{ok, {mime, MimeType}, Body} when is_binary (MimeType) ->
			respond_with_content ({mime, MimeType}, Body, Request, State);
		{error, Reason} ->
			respond_with_content (error, Reason, Request, State)
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
	
encode_content (json, Object) ->
	case mosaic_json_coders:encode_json (Object) of
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


enforce_get_request (Arguments, Request) ->
	case wrq:method (Request) of
		'GET' ->
			case enforce_arguments (Arguments, Request) of
				{ok, false, ArgumentValues} ->
					case ArgumentValues of
						[] ->
							{ok, false};
						_ ->
							{ok, false, ArgumentValues}
					end;
				Error = {error, true, _Reason} ->
					Error
			end;
		OtherMethod ->
			{error, true, {invalid_method, OtherMethod}}
	end.


enforce_post_request (Arguments, Body, Request) ->
	case wrq:method (Request) of
		'POST' ->
			case enforce_arguments (Arguments, Request) of
				{ok, false, ArgumentValues} ->
					case enforce_body (Body, Request) of
						{ok, false, BodyValue} ->
							case ArgumentValues of
								[] ->
									{ok, false, BodyValue};
								_ ->
									{ok, false, ArgumentValues, BodyValue}
							end;
						Error = {error, true, _Reason} ->
							Error
					end;
				Error = {error, true, _Reason} ->
					Error
			end;
		OtherMethod ->
			{error, true, {invalid_method, OtherMethod}}
	end.


enforce_arguments (Arguments, Request)
		when is_list (Arguments) ->
	case parse_arguments (Arguments, Request) of
		{ok, ArgumentNames, ArgumentValues} ->
			case lists:filter (
					fun (Name) -> not lists:member (Name, ArgumentNames) end,
					lists:map (fun ({Name, _}) -> erlang:list_to_binary (Name) end, wrq:req_qs (Request))) of
				[] ->
					{ok, false, ArgumentValues};
				UnexpectedArgumentNames ->
					{error, true, {unexpected_arguments, UnexpectedArgumentNames}}
			end;
		{error, Reason} ->
			{error, true, Reason}
	end.

parse_arguments (Arguments, Request) ->
	case parse_arguments (Arguments, Request, [], []) of
		{ok, Names, Values} ->
			{ok, lists:reverse (Names), lists:reverse (Values)};
		Error = {error, _Reason} ->
			Error
	end.

parse_arguments ([], _Request, Names, Values) ->
	{ok, Names, Values};
	
parse_arguments ([Name | Arguments], Request, Names, Values)
		when is_binary (Name) ->
	case wrq:get_qs_value (erlang:binary_to_list (Name), Request) of
		Value when is_list (Value) ->
			parse_arguments (Arguments, Request, [Name | Names], [erlang:list_to_binary (Value) | Values]);
		undefined ->
			{error, {missing_argument, Name}}
	end;
	
parse_arguments ([{Name, Parser} | Arguments], Request, Names, Values)
		when is_binary (Name), is_function (Parser, 1) ->
	case wrq:get_qs_value (erlang:binary_to_list (Name), Request) of
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


enforce_body (MimeType, Request)
		when is_atom (MimeType) ->
	enforce_body ({MimeType}, Request);
	
enforce_body (MimeType, Request)
		when is_binary (MimeType) ->
	enforce_body ({MimeType}, Request);
	
enforce_body ({json}, Request) ->
	enforce_body ({<<"application/json">>, fun mosaic_json_coders:decode_json/1}, Request);
	
enforce_body ({json, Parser}, Request)
		when is_function (Parser, 1) ->
	enforce_body ({<<"application/json">>, Parser}, Request);
	
enforce_body ({MimeType}, Request)
		when is_binary (MimeType) ->
	case wrq:get_req_header ("Content-Type", Request) of
		EncodedContentType when is_list (EncodedContentType) ->
			case erlang:list_to_binary (EncodedContentType) of
				MimeType ->
					case wrq:req_body (Request) of
						Body when is_binary (Body) ->
							{ok, false, Body};
						undefined ->
							{error, true, missing_body}
					end;
				OtherMimeType ->
					{error, true, {invalid_content_type, OtherMimeType}}
			end;
		undefined ->
			{error, true, missing_content_type}
	end;
	
enforce_body ({MimeType, Parser}, Request)
		when is_binary (MimeType), is_function (Parser, 1) ->
	case wrq:get_req_header ("Content-Type", Request) of
		EncodedContentType when is_list (EncodedContentType) ->
			case erlang:list_to_binary (EncodedContentType) of
				MimeType ->
					case wrq:req_body (Request) of
						Body when is_binary (Body) ->
							case Parser (Body) of
								{ok, BodyTerm} ->
									{ok, false, BodyTerm};
								{error, Reason} ->
									{error, true, {invalid_body, Reason}}
							end;
						undefined ->
							{error, true, missing_body}
					end;
				OtherMimeType ->
					{error, true, {invalid_content_type, OtherMimeType}}
			end;
		undefined ->
			{error, true, missing_content_type}
	end.
