
-module (mosaic_json_coders).


-export ([encode_json/1, decode_json/1, coerce_json/1, validate_json/1, validate_json/2]).


encode_json (Json) ->
	try
		{ok, erlang:iolist_to_binary (mochijson2:encode (Json))}
	catch
		throw : Reason -> {error, {invalid_json, Json, {mochijson2, Reason}}};
		error : Reason -> {error, {invalid_json, Json, {mochijson2, Reason}}};
		exit : Reason -> {error, {invalid_json, Json, {mochijson2, Reason}}}
	end.


decode_json (Json)
		when is_binary (Json); is_list (Json) ->
	try
		if
			is_binary (Json) ->
				{ok, mochijson2:decode (erlang:binary_to_list (Json))};
			is_list (Json) ->
				{ok, mochijson2:decode (Json)}
		end
	catch
		throw : Reason -> {error, {invalid_json, Json, {mochijson2, Reason}}};
		error : Reason -> {error, {invalid_json, Json, {mochijson2, Reason}}};
		exit : Reason -> {error, {invalid_json, Json, {mochijson2, Reason}}}
	end.


coerce_json (Json) ->
	try
		{ok, coerce_json_ok_1 (Json)}
	catch
		throw : {error, Reason} -> {error, {invalid_json, Json, Reason}};
		throw : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}};
		error : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}};
		exit : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}}
	end.

coerce_json_ok_1 (Json)
		when is_binary (Json); is_integer (Json); is_float (Json); is_boolean (Json); (Json =:= null) ->
	Json;
	
coerce_json_ok_1 (Json)
		when is_atom (Json) ->
	erlang:atom_to_binary (Json, utf8);
	
coerce_json_ok_1 (Json)
		when is_list (Json) ->
	lists:map (fun coerce_json_ok_1/1, Json);
	
coerce_json_ok_1 ({struct, Attributes})
		when is_list (Attributes) ->
	{struct, lists:map (fun coerce_json_attribute_ok_1/1, Attributes)};
	
coerce_json_ok_1 (Json) ->
	throw ({error, {invalid_value, Json}}).

coerce_json_attribute_ok_1 ({Key, Json})
		when is_binary (Key); is_integer (Key); is_float (Key); is_boolean (Key); (Key =:= null) ->
	{Key, coerce_json_ok_1 (Json)};
	
coerce_json_attribute_ok_1 ({Key, Json})
		when is_atom (Key) ->
	{erlang:atom_to_binary (Key, utf8), coerce_json_ok_1 (Json)};
	
coerce_json_attribute_ok_1 (Attribute) ->
	throw ({error, {invalid_struct_attribute, Attribute}}).


validate_json (Json) ->
	try
		ok = validate_json_ok (Json)
	catch
		throw : {error, Reason} -> {error, {invalid_json, Json, Reason}};
		throw : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}};
		error : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}};
		exit : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}}
	end.

validate_json_ok (Json)
		when is_binary (Json); is_integer (Json); is_float (Json); is_boolean (Json); (Json =:= null) ->
	ok;
	
validate_json_ok (Json)
		when is_list (Json) ->
	ok = lists:foreach (fun validate_json_ok/1, Json);
	
validate_json_ok ({struct, Attributes})
		when is_list (Attributes) ->
	ok = lists:foreach (fun validate_json_attribute_ok/1, Attributes);
	
validate_json_ok (Json) ->
	throw ({error, {invalid_value, Json}}).

validate_json_attribute_ok ({Key, Json})
		when is_binary (Key); is_integer (Key); is_float (Key); is_boolean (Key); (Key =:= null) ->
	ok = validate_json_ok (Json);
	
validate_json_attribute_ok (Attribute) ->
	throw ({error, {invalid_struct_attribute, Attribute}}).


validate_json (Json, Schema) ->
	try
		ok = validate_json_ok (Json, Schema)
	catch
		throw : Error = {error, _Reason} -> Error;
		throw : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}};
		error : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}};
		exit : Reason -> {error, {invalid_json, Json, {unexpected_error, Reason, erlang:get_stacktrace ()}}}
	end.

validate_json_ok (Json, is_json) ->
	validate_json_ok (Json, {is_json, undefined});
	
validate_json_ok (Json, is_string) ->
	validate_json_ok (Json, {is_string, undefined});
	
validate_json_ok (Json, is_boolean) ->
	validate_json_ok (Json, {is_boolean, undefined});
	
validate_json_ok (Json, is_integer) ->
	validate_json_ok (Json, {is_integer, undefined});
	
validate_json_ok (Json, is_float) ->
	validate_json_ok (Json, {is_float, undefined});
	
validate_json_ok (Json, is_null) ->
	validate_json_ok (Json, {is_null, undefined});
	
validate_json_ok (Json, is_list) ->
	validate_json_ok (Json, {is_list, undefined});
	
validate_json_ok (Json, is_struct) ->
	validate_json_ok (Json, {is_struct, undefined});
	
validate_json_ok (Json, {is_json, Reason}) ->
	try
		ok = validate_json_ok (Json)
	catch throw : {error, NestedReason} -> validate_json_error (Json, Reason, invalid_json, NestedReason) end;
	
validate_json_ok (Json, {is_string, Reason}) ->
	if
		is_binary (Json) -> ok;
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_string, undefined)
	end;
	
validate_json_ok (Json, {is_boolean, Reason}) ->
	if
		is_boolean (Json) -> ok;
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_boolean, undefined)
	end;
	
validate_json_ok (Json, {is_integer, Reason}) ->
	if
		is_integer (Json) -> ok;
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_integer, undefined)
	end;
	
validate_json_ok (Json, {is_float, Reason}) ->
	if
		is_float (Json) -> ok;
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_float, undefined)
	end;
	
validate_json_ok (Json, {is_null, Reason}) ->
	if
		(Json =:= null) -> ok;
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_null, undefined)
	end;
	
validate_json_ok (Json, {is_list, Reason}) ->
	validate_json_ok (Json, {is_list, [], Reason});
	
validate_json_ok (Json, {is_list, Constraints, Reason})
		when is_list (Constraints) ->
	if
		is_list (Json) ->
			ok = lists:foreach (
					fun
						({length, Length}) when is_integer (Length), (Length >= 0) ->
							if
								(length (Json) =:= Length) -> ok;
								true -> validate_json_error (Json, Reason, invalid_list, {expected_length, Length})
							end;
						({element, ElementSchema}) ->
							ok = lists:foreach (
									fun (Element) ->
										try
											ok = validate_json_ok (Element, ElementSchema)
										catch throw : {error, NestedReason} -> validate_json_error (Json, Reason, invalid_list, NestedReason) end
									end,
									Json);
						({elements, ElementSchemas}) when is_list (ElementSchemas) ->
							Length = erlang:length (ElementSchemas),
							if
								(length (Json) =:= Length) ->
									ok = lists:foreach (
											fun ({Element, ElementSchema}) ->
												try
													ok = validate_json_ok (Element, ElementSchema)
												catch throw : {error, NestedReason} -> validate_json_error (Json, Reason, invalid_list, NestedReason) end
											end,
											lists:zip (Json, ElementSchemas));
								true -> validate_json_error (Json, Reason, invalid_list, {expected_length, Length})
							end
					end,
					Constraints);
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_list, undefined)
	end;
	
validate_json_ok (Json, {is_struct, Reason}) ->
	validate_json_ok (Json, {is_struct, [], Reason});
	
validate_json_ok (Json, {is_struct, Constraints, Reason})
		when is_list (Constraints) ->
	case Json of
		{struct, Attributes} when is_list (Attributes) ->
			ok = lists:foreach (
					fun
						({size, Size}) when is_integer (Size), (Size >= 0) ->
							if
								(length (Attributes) =:= Size) -> ok;
								true -> validate_json_error (Json, Reason, invalid_struct, {expected_size, Size})
							end;
						({attribute, AttributeSchema}) ->
							try
								ok = lists:foreach (fun validate_json_attribute_ok/1, Attributes)
							catch throw : {error, NestedReason1} -> validate_json_error (Json, Reason, invalid_struct, NestedReason1) end,
							ok = lists:foreach (
									fun ({_Key, Attribute}) ->
										try
											ok = validate_json_ok (Attribute, AttributeSchema)
										catch throw : {error, NestedReason2} -> validate_json_error (Json, Reason, invalid_struct, NestedReason2) end
									end,
									Attributes);
						({attributes, AttributesSchema}) ->
							try
								ok = lists:foreach (fun validate_json_attribute_ok/1, Attributes)
							catch throw : {error, NestedReason1} -> validate_json_error (Json, Reason, invalid_struct, NestedReason1) end,
							SortedAttributes = dict:from_list (Attributes),
							SortedAttributesSchema = dict:from_list (AttributesSchema),
							SortedKeys = sets:union (
									sets:from_list (dict:fetch_keys (SortedAttributes)),
									sets:from_list (dict:fetch_keys (SortedAttributesSchema))),
							ok = lists:foreach (
									fun (Key) ->
										Attribute = case dict:find (Key, SortedAttributes) of
											{ok, Attribute_} -> Attribute_;
											error -> validate_json_error (Json, Reason, invalid_struct, {expected_attribute, Key})
										end,
										AttributeSchema = case dict:find (Key, SortedAttributesSchema) of
											{ok, Schema_} -> Schema_;
											error -> validate_json_error (Json, Reason, invalid_struct, {unexpected_attribute, Key})
										end,
										try
											ok = validate_json_ok (Attribute, AttributeSchema)
										catch throw : {error, NestedReason2} -> validate_json_error (Json, Reason, invalid_struct, NestedReason2) end
									end,
									sets:to_list (SortedKeys))
					end,
					Constraints);
		_ ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, expected_struct, undefined)
	end;
	
validate_json_ok (Json, {'andalso', Schemas, Reason})
		when is_list (Schemas), (Schemas =/= []) ->
	try
		ok = lists:foreach (
				fun (Schema) ->
					ok = validate_json_ok (Json, Schema)
				end,
				Schemas)
	catch throw : {error, NestedReason} -> validate_json_error (Json, Reason, unexpected_json, NestedReason) end;
	
validate_json_ok (Json, {'orelse', Schemas, Reason})
		when is_list (Schemas), (Schemas =/= []) ->
	try
		ok = lists:foreach (
				fun (Schema) ->
					try
						ok = validate_json_ok (Json, Schema),
						throw (ok)
					catch throw : {error, _Reason} -> ok end
				end,
				Schemas),
		validate_json_error (Json, Reason, unexpected_json, undefined)
	catch ok -> ok end;
	
validate_json_ok (Json, {equals, Expected}) ->
	validate_json_ok (Json, {equals, Expected, undefined});
	
validate_json_ok (Json, {equals, Expected, Reason}) ->
	if
		(Json =:= Expected) -> ok;
		true ->
			ok = validate_json_ok (Json, {is_json, Reason}),
			validate_json_error (Json, Reason, unexpected_json, {expected_json, Expected})
	end.

validate_json_error (Json, undefined, undefined, undefined) ->
	throw ({error, {invalid_json, Json}});
	
validate_json_error (Json, undefined, DefaultReason, undefined)
		when is_atom (DefaultReason) ->
	throw ({error, {DefaultReason, Json}});
	
validate_json_error (Json, undefined, DefaultReason, NestedReason)
		when is_atom (DefaultReason) ->
	throw ({error, {DefaultReason, Json, NestedReason}});
	
validate_json_error (Json, Reason, DefaultReason, undefined)
		when is_atom (Reason), is_atom (DefaultReason) ->
	throw ({error, {Reason, Json}});
	
validate_json_error (Json, Reason, DefaultReason, NestedReason)
		when is_atom (Reason), is_atom (DefaultReason) ->
	throw ({error, {Reason, Json, NestedReason}}).
