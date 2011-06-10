
-module (mosaic_json_coders).


-export ([encode_json/1, decode_json/1, coerce_json/1]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


encode_json (Json) ->
	try
		{ok, erlang:list_to_binary (mochijson2:encode (Json))}
	catch
		throw : Reason -> {error, {invalid_json, Json, Reason}};
		error : Reason -> {error, {invalid_json, Json, Reason}};
		exit : Reason -> {error, {invalid_json, Json, Reason}}
	end.


decode_json (Data)
		when is_binary (Data); is_list (Data) ->
	try
		if
			is_binary (Data) ->
				{ok, mochijson2:decode (erlang:binary_to_list (Data))};
			is_list (Data) ->
				{ok, mochijson2:decode (Data)}
		end
	catch
		throw : Reason -> {error, {invalid_json, Data, Reason}};
		error : Reason -> {error, {invalid_json, Data, Reason}};
		exit : Reason -> {error, {invalid_json, Data, Reason}}
	end.


coerce_json (Json) ->
	try
		{ok, coerce_json_ok_1 (Json)}
	catch
		throw : {error, Reason} -> {error, {invalid_json, Json, Reason}};
		throw : Reason -> {error, {invalid_json, Json, Reason}};
		error : Reason -> {error, {invalid_json, Json, Reason}}
	end.

coerce_json_ok_1 (Value)
		when is_binary (Value); is_integer (Value); is_float (Value); is_boolean (Value); (Value =:= null) ->
	Value;
	
coerce_json_ok_1 (Atom)
		when is_atom (Atom) ->
	erlang:atom_to_binary (Atom, utf8);
	
coerce_json_ok_1 (List)
		when is_list (List) ->
	lists:map (fun coerce_json_ok_1/1, List);
	
coerce_json_ok_1 ({struct, Attributes})
		when is_list (Attributes) ->
	{struct, lists:map (fun coerce_json_attribute_ok_1/1, Attributes)};
	
coerce_json_ok_1 (Value) ->
	throw ({error, {invalid_value, Value}}).

coerce_json_attribute_ok_1 ({Key, Value})
		when is_binary (Key); is_integer (Key); is_float (Key); is_boolean (Key); (Key =:= null) ->
	{Key, coerce_json_ok_1 (Value)};
	
coerce_json_attribute_ok_1 ({Atom, Value})
		when is_atom (Atom) ->
	{erlang:atom_to_binary (Atom, utf8), coerce_json_ok_1 (Value)};
	
coerce_json_attribute_ok_1 (Attribute) ->
	throw ({error, {invalid_attribute, Attribute}}).
