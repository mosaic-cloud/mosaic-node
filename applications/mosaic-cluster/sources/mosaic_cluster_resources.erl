
-module (mosaic_cluster_resources).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_sup:start_child_daemon (mosaic_cluster_resources, {local, mosaic_component_resources}, [Configuration], permanent).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_cluster_resources, QualifiedName, Configuration).


-record (state, {qualified_name, table}).


init ({QualifiedName, defaults}) ->
	case mosaic_process_tools:ensure_registered (QualifiedName) of
		ok ->
			Table = ets:new (mosaic_cluster_resources, [set, protected, named_table]),
			State = #state{qualified_name = QualifiedName, table = Table},
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{}) ->
	ok.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_component_resources, acquire, OwnerIdentifier, OwnerProcess, SpecificationEncoding, SpecificationContent}, _Sender, State = #state{table = Table})
		when is_binary (OwnerIdentifier), (bit_size (OwnerIdentifier) =:= 160), (is_pid (OwnerProcess) orelse is_port (OwnerProcess)) ->
	case parse_resource_specifications (SpecificationEncoding, SpecificationContent) of
		{ok, Specifications} ->
			case execute_acquire (OwnerIdentifier, OwnerProcess, Specifications, Table) of
				{ok, Descriptors} ->
					case format_resource_descriptors (SpecificationEncoding, Descriptors) of
						{ok, DescriptorsContent} ->
							{reply, {ok, DescriptorsContent}, State};
						Error = {error, _Reason} ->
							{reply, Error, State}
					end;
				Error = {error, _Reason} ->
					{reply, Error, State}
			end;
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_call (Request, Sender, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast (Request, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid request; ignoring!", [{request, Request}]),
	{noreply, State}.


handle_info (Message, State = #state{}) ->
	ok = mosaic_transcript:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{noreply, State}.


execute_acquire (OwnerIdentifier, OwnerProcess, Specifications, Table) ->
	try
		Cache = lists:foldl (
				fun (Specification = {Identifier, Type, Attributes}, OldCache) ->
					case Type of
						'socket:ipv4:tcp' ->
							case Attributes of
								[] ->
									{ok, NewCache} = try_acquire_resource (
											fun () ->
												Ip = <<"127.0.0.1">>,
												Port = crypto:rand_uniform (32769, 49150),
												Key = {Type, Ip, Port},
												Data = {Identifier, Type, OwnerIdentifier, OwnerProcess, {Ip, Port}},
												Descriptor = {Identifier, [{ip, Ip}, {port, Port}]},
												{ok, {Key, Data, Descriptor, Specification}}
											end,
											Table, OldCache),
									NewCache;
								_ ->
									throw ({error, {invalid_resource_attributes, Attributes}})
							end;
						_ ->
							throw ({error, {invalid_resource_type, Type}})
					end
				end, orddict:new (), Specifications),
		Descriptors = orddict:fold (
				fun (Key, Record = {Key, _Data, Descriptor, _Specification}, Descriptors) ->
					true = ets:insert (Table, Record),
					[Descriptor | Descriptors]
				end, [], Cache),
		{ok, Descriptors}
	catch
		throw : Error = {error, _Reason} ->
			Error
	end.


try_acquire_resource (Generator, Table, OldCache) ->
	{ok, Record = {Key, _Data, _Descriptor, _Specification}} = Generator (),
	case ets:lookup (Table, Key) of
		[] ->
			case orddict:is_key (Key, OldCache) of
				false ->
					NewCache = orddict:store (Key, Record, OldCache),
					{ok, NewCache};
				true ->
					try_acquire_resource (Generator, Table, OldCache)
			end;
		_ ->
			try_acquire_resource (Generator, Table, OldCache)
	end.


format_resource_descriptors (term, Descriptors)
		when is_list (Descriptors) ->
	{ok, lists:map (
			fun (Descriptor) ->
				format_resource_descriptor (term, Descriptor)
			end, Descriptors)};
	
format_resource_descriptors (json, Descriptors)
		when is_list (Descriptors) ->
	{ok, {struct, lists:map (
			fun (Descriptor) ->
				format_resource_descriptor (json, Descriptor)
			end, Descriptors)}}.


format_resource_descriptor (term, {Identifier, Attributes})
		when is_atom (Identifier), is_list (Attributes) ->
	{Identifier,
			lists:map (
					fun ({AttributeName, AttributeValue}) ->
						format_resource_attribute (term, AttributeName, AttributeValue)
					end, Attributes)};
	
format_resource_descriptor (json, {Identifier, Attributes})
		when is_atom (Identifier), is_list (Attributes) ->
	{erlang:atom_to_binary (Identifier, 'utf8'),
			lists:map (
					fun ({AttributeName, AttributeValue}) ->
						format_resource_attribute (json, AttributeName, AttributeValue)
					end, Attributes)}.

format_resource_attribute (term, Name, Value)
		when is_atom (Name) ->
	if
		is_atom (Value) ->
			{Name, Value};
		is_number (Value) ->
			{Name, Value};
		is_binary (Value) ->
			{Name, Value}
	end;
	
format_resource_attribute (json, Name, Value)
		when is_atom (Name) ->
	if
		(Value =:= true); (Value =:= false); (Value =:= null) ->
			{erlang:atom_to_binary (Name, 'utf8'), Value};
		is_atom (Value) ->
			{erlang:atom_to_binary (Name, 'utf8'), erlang:atom_to_binary (Value, 'utf8')};
		is_number (Value) ->
			{erlang:atom_to_binary (Name, 'utf8'), Value};
		is_binary (Value) ->
			{erlang:atom_to_binary (Name, 'utf8'), Value}
	end.


parse_resource_specifications (term, Specifications)
		when is_list (Specifications) ->
	try
		Resources = lists:map (
				fun ({Identifier, Specification}) ->
					parse_resource_specification (Identifier, term, Specification)
				end, Specifications),
		{ok, Resources}
	catch
		throw : Error = {error, _Reason} ->
			Error
	end;
	
parse_resource_specifications (json, {struct, Specifications})
		when is_list (Specifications) ->
	try
		Resources = lists:map (
				fun ({Identifier, Specification}) ->
					parse_resource_specification (Identifier, json, Specification)
				end, Specifications),
		{ok, Resources}
	catch
		throw : Error = {error, _Reason} ->
			Error
	end;
	
parse_resource_specifications (Encoding, Specifications) ->
	{error, {invalid_resource_specifications, Encoding, Specifications}}.


parse_resource_specification (Identifier, term, Type)
		when is_atom (Identifier), is_atom (Type) ->
	{Identifier, Type, []};
	
parse_resource_specification (Identifier, json, Type)
		when is_binary (Identifier), is_binary (Type) ->
	parse_resource_specification (erlang:binary_to_atom (Identifier, 'utf8'), term, erlang:binary_to_atom (Type, 'utf8'));
	
parse_resource_specification (Identifier, Encoding, Specification) ->
	throw ({error, {invalid_resource_specification, {Encoding, Identifier, Specification}}}).
