
-module (mosaic_webmachine).

-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/0, start_supervised/1]).
-export ([return_with_outcome/3, respond_with_outcome/3]).
-export ([return_with_content/5, respond_with_content/4]).
-export ([enforce_request/3]).
-export ([
		parse_existing_atom/1, parse_integer/1, parse_float/1,
		parse_hex_string/1,
		parse_integer_identifier/1, parse_string_identifier/1,
		parse_json/1]).
-export ([
		format_atom/1, format_integer/1, format_float/1, format_term/1,
		format_hex_string/1,
		format_integer_identifier/1, format_string_identifier/1,
		format_json/1]).


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
	mosaic_sup:start_child_daemon (mosaic_webmachine, {local, mosaic_webmachine}, [Configuration], permanent).


options (defaults) ->
	case application:get_env (mosaic_cluster, webmachine_listen) of
		{ok, {Address, Port}} when is_list (Address), is_number (Port), (Port >= 0), (Port < 65536) ->
			case dispatches ([mosaic_console_wm, mosaic_cluster_wm, mosaic_executor_wm]) of
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


encode_content (json, Content) ->
	{ok, "application/json", format_json (Content)};
	
encode_content (error, Reason) ->
	encode_content (json, {struct, [{ok, false}, {error, format_term (Reason)}]}).


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


parse_existing_atom (Binary)
		when is_binary (Binary) ->
	parse_existing_atom (erlang:binary_to_list (Binary));
	
parse_existing_atom (String)
		when is_list (String) ->
	try
		{ok, erlang:list_to_existing_atom (String)}
	catch
		error : badarg ->
			{error, {inexistent_atom, String}}
	end.

parse_integer (Binary)
		when is_binary (Binary) ->
	parse_integer (erlang:binary_to_list (Binary));
	
parse_integer (String)
		when is_list (String) ->
	try
		{ok, erlang:list_to_integer (String)}
	catch
		error : badarg ->
			{error, {invalid_integer, String}}
	end.

parse_float (Binary)
		when is_binary (Binary) ->
	parse_float (erlang:binary_to_list (Binary));
	
parse_float (String)
		when is_list (String) ->
	try
		{ok, erlang:list_to_float (String)}
	catch
		error : badarg ->
			{error, {invalid_float, String}}
	end.

parse_hex_string (Binary)
		when is_binary (Binary) ->
	parse_hex_string (erlang:binary_to_list (Binary));
	
parse_hex_string (String)
		when is_list (String) ->
	try
		StringLength = erlang:length (String),
		ok = if
			((StringLength rem 2) =:= 0) ->
				ok;
			true ->
				throw ({error, {invalid_length, StringLength}})
		end,
		FinalBinarySize = (StringLength div 2),
		Integer = erlang:list_to_integer (String, 16),
		IntegerBinary = binary:encode_unsigned (Integer),
		IntegerBinarySize = erlang:byte_size (IntegerBinary),
		if
			(FinalBinarySize =:= IntegerBinarySize) ->
				{ok, IntegerBinary};
			(FinalBinarySize > IntegerBinarySize) ->
				{ok, <<0 : ((FinalBinarySize - IntegerBinarySize) * 8), IntegerBinary / binary>>};
			true ->
				throw ({error, {invalid_length, StringLength}})
		end
	catch
		throw : Error = {error, _Reason} ->
			Error;
		error : Reason ->
			{error, Reason}
	end.

parse_string_identifier (Binary)
		when is_binary (Binary) ->
	parse_string_identifier (erlang:binary_to_list (Binary));
	
parse_string_identifier (String)
		when is_list (String) ->
	case parse_hex_string (String) of
		Outcome = {ok, Identifier} ->
			if
				(erlang:bit_size (Identifier) =:= 160) ->
					Outcome;
				true ->
					{error, {invalid_length, erlang:length (String)}}
			end;
		Error = {error, _Reason} ->
			Error
	end.

parse_integer_identifier (Binary)
		when is_binary (Binary) ->
	parse_integer_identifier (erlang:binary_to_list (Binary));
	
parse_integer_identifier (String)
		when is_list (String) ->
	case parse_string_identifier (String) of
		{ok, <<Identifier : 160>>} ->
			{ok, Identifier};
		Error = {error, _Reason} ->
			Error
	end.

parse_json (Binary)
		when is_binary (Binary) ->
	parse_json (erlang:binary_to_list (Binary));
	
parse_json (String)
		when is_list (String) ->
	try
		{ok, mochijson2:decode (String)}
	catch
		error : _ ->
			{error, {invalid_json, String}}
	end.


format_atom (Atom)
		when is_atom (Atom) ->
	erlang:list_to_binary (erlang:atom_to_list (Atom)).

format_integer (Integer)
		when is_integer (Integer) ->
	erlang:list_to_binary (erlang:integer_to_list (Integer)).

format_float (Float)
		when is_float (Float) ->
	erlang:list_to_binary (erlang:float_to_list (Float)).

format_hex_string (Binary)
		when is_binary (Binary) ->
	format_hex_string (erlang:binary_to_list (Binary));
	
format_hex_string (String)
		when is_list (String) ->
	erlang:iolist_to_binary (lists:flatten ([io_lib:format ("~2.16.0b", [Byte]) || Byte <- String])).

format_string_identifier (Identifier)
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	format_hex_string (Identifier).

format_integer_identifier (Identifier)
		when is_integer (Identifier), (Identifier >= 0), (Identifier < 1461501637330902918203684832716283019655932542976) ->
	format_string_identifier (<<Identifier : 160>>).

format_json (Json) ->
	mochijson2:encode (Json).

format_term (Term) ->
	erlang:iolist_to_binary (format_term_1 (Term)).

format_term_1 (Atom)
		when is_atom (Atom) ->
	[$', erlang:atom_to_list (Atom), $'];
	
format_term_1 (Integer)
		when is_integer (Integer) ->
	erlang:integer_to_list (Integer);
	
format_term_1 (Float)
		when is_float (Float) ->
	erlang:float_to_list (Float);
	
format_term_1 (List)
		when is_list (List) ->
	Ascii = lists:all (fun (Byte) when is_integer (Byte), (Byte >= 32), (Byte =< 127) -> true; (_) -> false end, List),
	if
		Ascii ->
			[$", io_lib:format ("~s", [List]), $"];
		true ->
			[$[, string:join ([format_term_1 (Element) || Element <- List], ", "), $]]
	end;
	
format_term_1 (Tuple)
		when is_tuple (Tuple) ->
	[${, string:join ([format_term_1 (Element) || Element <- erlang:tuple_to_list (Tuple)], ", "), $}];
	
format_term_1 (<<>>) ->
	"<<>>";
	
format_term_1 (Binary)
		when is_binary (Binary) ->
	["<<16#", [io_lib:format ("~2.16.0b", [Byte]) || Byte <- binary:bin_to_list (Binary)], ":", erlang:integer_to_list (erlang:bit_size (Binary)), ">>"];
	
format_term_1 (Object) ->
	["binary_to_term(", format_term_1 (erlang:term_to_binary (Object)), ")"].
