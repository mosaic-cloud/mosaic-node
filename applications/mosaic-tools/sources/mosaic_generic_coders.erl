
-module (mosaic_generic_coders).


-export ([
		encode_atom/1, decode_atom/1,
		encode_integer/1, decode_integer/1,
		encode_float/1, decode_float/1,
		encode_hex_data/1, decode_hex_data/1,
		encode_string/1, decode_string/1,
		encode_term/2,
		encode_reason/2]).
-export ([
		generate_data/1, generate_hex_data/1]).
-export ([
		validate_term/2]).
-export ([
		proplist_get/4, proplist_pop/4,
		application_env_get/4,
		os_env_get/3, os_env_get/1,
		os_bin_get/2, os_bin_get/1]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


encode_atom (Atom)
		when is_atom (Atom) ->
	{ok, erlang:atom_to_binary (Atom, utf8)}.

decode_atom (Data)
		when is_binary (Data); is_list (Data) ->
	try
		if
			is_binary (Data) ->
				{ok, erlang:binary_to_existing_atom (Data, utf8)};
			is_list (Data) ->
				{ok, erlang:list_to_existing_atom (Data)}
		end
	catch error : badarg -> {error, {inexistent_atom, Data}} end.


encode_integer (Integer)
		when is_integer (Integer) ->
	{ok, erlang:list_to_binary (erlang:integer_to_list (Integer))}.

decode_integer (Binary)
		when is_binary (Binary) ->
	decode_integer (erlang:binary_to_list (Binary));
	
decode_integer (String)
		when is_list (String) ->
	try
		{ok, erlang:list_to_integer (String)}
	catch error : badarg -> {error, {invalid_integer, String}} end.


encode_float (Float)
		when is_float (Float) ->
	{ok, erlang:list_to_binary (erlang:float_to_list (Float))}.

decode_float (Binary)
		when is_binary (Binary) ->
	decode_float (erlang:binary_to_list (Binary));
	
decode_float (String)
		when is_list (String) ->
	try
		{ok, erlang:list_to_float (String)}
	catch error : badarg -> {error, {invalid_float, String}} end.


encode_string (Binary)
		when is_binary (Binary) ->
	{ok, Binary};
	
encode_string (String)
		when is_list (String) ->
	try
		{ok, erlang:iolist_to_binary (String)}
	catch error : badarg -> {error, {invalid_string, String}} end.

decode_string (Binary)
		when is_binary (Binary) ->
	{ok, Binary};
	
decode_string (String)
		when is_list (String) ->
	try
		{ok, erlang:iolist_to_binary (String)}
	catch error : badarg -> {error, {invalid_string, String}} end.


encode_hex_data (Binary)
		when is_binary (Binary) ->
	encode_hex_data (erlang:binary_to_list (Binary));
	
encode_hex_data (Bytes)
		when is_list (Bytes) ->
	try
		{ok, erlang:iolist_to_binary (
					[io_lib:format ("~2.16.0b", [Byte]) || Byte <- Bytes,
								((is_integer(Byte) andalso (Byte >= 0) andalso (Byte =< 256))
											orelse throw ({error, {invalid_byte, Byte}}))])}
	catch throw : {error, Reason} -> {error, {invalid_hex_data, Bytes, Reason}} end.


decode_hex_data (Binary)
		when is_binary (Binary) ->
	decode_hex_data (erlang:binary_to_list (Binary));
	
decode_hex_data (String)
		when is_list (String) ->
	try
		StringLength = erlang:length (String),
		ok = if
			(StringLength > 0), ((StringLength rem 2) =:= 0) ->
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
		throw : {error, Reason} -> {error, {invalid_hex_data, String, Reason}};
		error : Reason -> {error, {invalid_hex_data, String, Reason}}
	end.


encode_text_term (Term) ->
	try
		{ok, erlang:iolist_to_binary (encode_text_term_ok_1 (Term))}
	catch throw : Error = {error, _Reason} -> Error end.

encode_text_term_ok_1 (Atom)
		when is_atom (Atom) ->
	[$', erlang:atom_to_binary (Atom, utf8), $'];
	
encode_text_term_ok_1 (Integer)
		when is_integer (Integer) ->
	erlang:integer_to_list (Integer);
	
encode_text_term_ok_1 (Float)
		when is_float (Float) ->
	erlang:float_to_list (Float);
	
encode_text_term_ok_1 (List)
		when is_list (List) ->
	Ascii = try
		[Byte || Byte <- List, ((is_integer (Byte) andalso (Byte >= 32) andalso (Byte =< 127)) orelse throw (false))],
		true
	catch throw : false -> false end,
	if
		Ascii ->
			[$", io_lib:format ("~s", [List]), $"];
		true ->
			[$[, string:join ([encode_text_term_ok_1 (Element) || Element <- List], ", "), $]]
	end;
	
encode_text_term_ok_1 (Tuple)
		when is_tuple (Tuple) ->
	[${, string:join ([encode_text_term_ok_1 (Element) || Element <- erlang:tuple_to_list (Tuple)], ", "), $}];
	
encode_text_term_ok_1 (<<>>) ->
	"<<>>";
	
encode_text_term_ok_1 (Binary)
		when is_binary (Binary) ->
	Ascii = try
		[Byte || <<Byte : 8>> <= Binary, ((is_integer (Byte) andalso (Byte >= 32) andalso (Byte =< 127)) orelse throw (false))],
		true
	catch throw : false -> false end,
	if
		Ascii ->
			["<<", $", Binary, $", ">>"];
		true ->
			["<<16#", [io_lib:format ("~2.16.0b", [Byte]) || <<Byte : 8>> <= Binary], ":", erlang:integer_to_list (erlang:bit_size (Binary)), ">>"]
	end;
	
encode_text_term_ok_1 (Object) ->
	["binary_to_term(", encode_text_term_ok_1 (erlang:term_to_binary (Object)), ")"].


encode_term (json, Term) ->
	encode_term (text, Term);
	
encode_term (text, Term) ->
	encode_text_term (Term).


encode_reason (json, Reason) ->
	encode_reason (text, Reason);
	
encode_reason (text, Reason) ->
	encode_text_term (Reason).


generate_data (Size)
		when is_integer (Size), (Size > 0) ->
	{ok, crypto:rand_bytes (Size)}.

generate_hex_data (Size)
		when is_integer (Size), (Size > 0) ->
	case generate_data (Size) of
		{ok, Data} ->
			encode_hex_data (Data);
		Error = {error, _Reason} ->
			Error
	end.


validate_term (Term, Schema) ->
	try
		validate_term_ok (Term, Schema)
	catch throw : Error = {error, _Reason} -> Error end.

validate_term_ok (Term, is_atom) ->
	if
		is_atom (Term) -> ok;
		true -> throw ({error, {invalid_atom, Term}})
	end;
	
validate_term_ok (Term, {is_atom, Reason}) ->
	if
		is_atom (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_atom}})
	end;
	
validate_term_ok (Term, is_binary) ->
	if
		is_binary (Term) -> ok;
		true -> throw ({error, {invalid_binary, Term}})
	end;
	
validate_term_ok (Term, {is_binary, Reason}) ->
	if
		is_binary (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_binary}})
	end;
	
validate_term_ok (Term, {is_binary, Size, Reason})
		when is_integer (Size), (Size >= 0) ->
	if
		is_binary (Term), (byte_size (Term) =:= Size) -> ok;
		is_binary (Term) -> throw ({error, {Reason, Term, mismatched_binary_size}});
		true -> throw ({error, {Reason, Term, invalid_binary}})
	end;
	
validate_term_ok (Term, is_bitstring) ->
	if
		is_bitstring (Term) -> ok;
		true -> throw ({error, {invalid_bitstring, Term}})
	end;
	
validate_term_ok (Term, {is_bitstring, Reason}) ->
	if
		is_bitstring (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_bitstring}})
	end;
	
validate_term_ok (Term, {is_bitstring, Size, Reason}) ->
	if
		is_bitstring (Term), (bit_size (Term) =:= Size) -> ok;
		is_bitstring (Term) -> throw ({error, {Reason, Term, mismatched_bitstring_size}});
		true -> throw ({error, {Reason, Term, invalid_bitstring}})
	end;
	
validate_term_ok (Term, is_boolean) ->
	if
		is_boolean (Term) -> ok;
		true -> throw ({error, {invalid_boolean, Term}})
	end;
	
validate_term_ok (Term, {is_boolean, Reason}) ->
	if
		is_boolean (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_boolean}})
	end;
	
validate_term_ok (Term, is_integer) ->
	if
		is_integer (Term) -> ok;
		true -> throw ({error, {invalid_integer, Term}})
	end;
	
validate_term_ok (Term, {is_integer, Reason}) ->
	if
		is_integer (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_integer}})
	end;
	
validate_term_ok (Term, {is_integer, {Min, Max}, Reason})
		when (is_integer (Min) orelse (Min =:= undefined)), (is_integer (Max) orelse (Max =:= undefined)) ->
	if
		is_integer (Term), ((Min =:= undefined) orelse (Term >= Min)), ((Max =:= undefined) orelse (Term =< Max)) -> ok;
		is_integer (Term) -> throw ({error, {Reason, Term, mismatched_integer_range}});
		true -> throw ({error, {Reason, Term, invalid_integer}})
	end;
	
validate_term_ok (Term, is_float) ->
	if
		is_float (Term) -> ok;
		true -> throw ({error, {invalid_float, Term}})
	end;
	
validate_term_ok (Term, {is_float, Reason}) ->
	if
		is_float (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_float}})
	end;
	
validate_term_ok (Term, {is_float, {Min, Max}, Reason})
		when (is_float (Min) orelse (Min =:= undefined)), (is_float (Max) orelse (Max =:= undefined)) ->
	if
		is_float (Term), ((Min =:= undefined) orelse (Term >= Min)), ((Max =:= undefined) orelse (Term =< Max)) -> ok;
		is_float (Term) -> throw ({error, {Reason, Term, mismatched_float_range}});
		true -> throw ({error, {Reason, Term, invalid_float}})
	end;
	
validate_term_ok (Term, {is_function, Reason}) ->
	if
		is_function (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_function}})
	end;
	
validate_term_ok (Term, {is_function, Arity, Reason})
		when is_integer (Arity), (Arity >= 0) ->
	if
		is_function (Term, Arity) -> ok;
		is_function (Term) -> throw ({error, {Reason, Term, mismatched_function_arity}});
		true -> throw ({error, {Reason, Term, invalid_function}})
	end;
	
validate_term_ok (Term, is_reference) ->
	if
		is_reference (Term) -> ok;
		true -> throw ({error, {invalid_reference, Term}})
	end;
	
validate_term_ok (Term, {is_reference, Reason}) ->
	if
		is_reference (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_reference}})
	end;
	
validate_term_ok (Term, is_list) ->
	if
		is_list (Term) -> ok;
		true -> throw ({error, {invalid_list, Term}})
	end;
	
validate_term_ok (Term, {is_list, Reason}) ->
	if
		is_list (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_list}})
	end;
	
validate_term_ok (Term, {is_list, Length, Reason})
		when is_integer (Length) ->
	if
		is_list (Term), (Length =:= length (Term)) -> ok;
		is_list (Term) -> throw ({error, {Reason, Term, mismatched_list_length}});
		true -> throw ({error, {Reason, Term, invalid_list}})
	end;
	
validate_term_ok (Term, {is_list, ElementSchema, Reason}) ->
	if
		is_list (Term) ->
			try
				ok = lists:foreach (fun (Element) -> ok = validate_term_ok (Element, ElementSchema) end, Term)
			catch throw : {error, ElementReason} -> throw ({error, {Reason, Term, ElementReason}}) end;
		true -> throw ({error, {Reason, Term, invalid_list}})
	end;
	
validate_term_ok (Term, is_tuple) ->
	if
		is_tuple (Term) -> ok;
		true -> throw ({error, {invalid_tuple, Term}})
	end;
	
validate_term_ok (Term, {is_tuple, Reason}) ->
	if
		is_tuple (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_tuple}})
	end;
	
validate_term_ok (Term, {is_tuple, Size, Reason})
		when is_integer (Size) ->
	if
		is_tuple (Term), (Size =:= tuple_size (Term)) -> ok;
		is_tuple (Term) -> throw ({error, {Reason, Term, mismatched_tuple_size}});
		true -> throw ({error, {Reason, Term, invalid_tuple}})
	end;
	
validate_term_ok (Term, {is_tuple, TupleSchema, Reason})
		when is_tuple (TupleSchema) ->
	if
		is_tuple (Term), (tuple_size (TupleSchema) =:= tuple_size (Term)) ->
			try
				ok = lists:foreach (fun ({Element, ElementSchema}) -> ok = validate_term_ok (Element, ElementSchema) end,
							lists:zip (erlang:tuple_to_list (Term), erlang:tuple_to_list (TupleSchema)))
			catch throw : {error, ElementReason} -> throw ({error, {Reason, Term, ElementReason}}) end;
		true -> throw ({error, {Reason, Term, invalid_tuple}})
	end;
	
validate_term_ok (Term, {is_record, Tag, Size, Reason})
		when is_atom (Tag), is_integer (Size) ->
	if
		is_tuple (Term), (Size =:= tuple_size (Term)), (Tag =:= element (1, Term)) -> ok;
		true -> throw ({error, {Reason, Term, invalid_record}})
	end;
	
validate_term_ok (Term, {is_record, RecordSchema, Reason})
		when is_tuple (RecordSchema), (tuple_size (RecordSchema) >= 2), is_atom (element (1, RecordSchema)) ->
	if
		is_tuple (Term), (tuple_size (RecordSchema) =:= tuple_size (Term)), (element (1, RecordSchema) =:= element (1, Term)) ->
			try
				ok = lists:foreach (fun ({Element, ElementSchema}) -> ok = validate_term_ok (Element, ElementSchema) end,
							erlang:tl (lists:zip (erlang:tuple_to_list (Term), erlang:tuple_to_list (RecordSchema))))
			catch throw : {error, ElementReason} -> throw ({error, {Reason, Term, ElementReason}}) end;
		true -> throw ({error, {Reason, Term, invalid_record}})
	end;
	
validate_term_ok (Term, is_pid) ->
	if
		is_pid (Term) -> ok;
		true -> throw ({error, {invalid_pid, Term}})
	end;
	
validate_term_ok (Term, {is_pid, Reason}) ->
	if
		is_pid (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_pid}})
	end;
	
validate_term_ok (Term, is_port) ->
	if
		is_port (Term) -> ok;
		true -> throw ({error, {invalid_port, Term}})
	end;
	
validate_term_ok (Term, {is_port, Reason}) ->
	if
		is_port (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_port}})
	end;
	
validate_term_ok (Term, is_process) ->
	if
		is_pid (Term); is_port (Term) -> ok;
		true -> throw ({error, {invalid_process, Term}})
	end;
	
validate_term_ok (Term, {is_process, Reason}) ->
	if
		is_pid (Term); is_port (Term) -> ok;
		true -> throw ({error, {Reason, Term, invalid_process}})
	end;
	
validate_term_ok (Term, {validator, Validator})
		when is_function (Validator, 1) ->
	case Validator (Term) of
		ok -> ok;
		{error, ValidatorReason} -> throw ({error, {ValidatorReason, Term}})
	end;
	
validate_term_ok (Term, {validator, Validator, Reason})
		when is_function (Validator, 1) ->
	case Validator (Term) of
		ok -> ok;
		{error, ValidatorReason} -> throw ({error, {Reason, Term, ValidatorReason}})
	end;
	
validate_term_ok (Term, {'andalso', Schemas, Reason})
		when is_list (Schemas), (Schemas =/= []) ->
	try
		ok = lists:foreach (fun (Schema) -> ok = validate_term_ok (Term, Schema) end, Schemas)
	catch throw : {error, AndalsoError} -> throw ({error, {Reason, Term, AndalsoError}}) end;
	
validate_term_ok (Term, {'orelse', Schemas, Reason})
		when is_list (Schemas), (Schemas =/= []) ->
	try
		ok = lists:foreach (
					fun (Schema) ->
						try
							ok = validate_term_ok (Term, Schema), throw (ok)
						catch throw : {error, _Reason} -> ok end
					end, Schemas),
		throw ({error, {Reason, Term}})
	catch ok -> ok end;
	
validate_term_ok (Term, {matches, Expected}) ->
	if
		(Term =:= Expected) -> ok;
		true -> throw ({error, {mismatched_value, Term}})
	end;
	
validate_term_ok (Term, {matches, Expected, Reason}) ->
	if
		(Term =:= Expected) -> ok;
		true -> throw ({error, {Reason, Term, mismatched_value}})
	end.


proplist_get (Key, Proplist, Enforce, Default)
		when is_list (Proplist) ->
	try
		case proplists:get_value (Key, Proplist, undefined) of
			undefined ->
				apply_default_get (Default);
			EncodedTerm ->
				apply_enforce_get (EncodedTerm, Enforce)
		end
	catch throw : Error = {error, _Reason} -> Error end.


proplist_pop (Key, Proplist, Enforce, Default)
		when is_list (Proplist) ->
	try
		Term = enforce_ok_1 (proplist_get (Key, Proplist, Enforce, Default)),
		RestProplist = proplists:delete (Key, Proplist),
		{ok, Term, RestProplist}
	catch throw : Error = {error, _Reason} -> Error end.


application_env_get (Key, Application, Enforce, Default)
		when is_atom (Key), is_atom (Application) ->
	try
		case application:get_env (Application, Key) of
			{ok, EncodedTerm} ->
				apply_enforce_get (EncodedTerm, Enforce);
			undefined ->
				apply_default_get (Default)
		end
	catch throw : Error = {error, _Reason} -> Error end.


os_env_get (Key) ->
	os_env_get (Key, {decode, fun decode_string/1}, {error, {unresolved_environment_variable, Key}}).

os_env_get (Key, Enforce, Default)
		when is_atom (Key) ->
	os_env_get (erlang:atom_to_list (Key), Enforce, Default);
	
os_env_get (Key, Enforce, Default)
		when is_binary (Key) ->
	os_env_get (erlang:binary_to_list (Key), Enforce, Default);
	
os_env_get (Key, Enforce, Default)
		when is_list (Key) ->
	try
		case os:getenv (Key) of
			false ->
				apply_default_get (Default);
			EncodedTerm ->
				apply_enforce_get (EncodedTerm, Enforce)
		end
	catch throw : Error = {error, _Reason} -> Error end.


os_bin_get (Key) ->
	os_bin_get (Key, {error, {unresolved_executable, Key}}).

os_bin_get (Key, Default)
		when is_atom (Key) ->
	os_bin_get (erlang:atom_to_list (Key), Default);
	
os_bin_get (Key, Default)
		when is_binary (Key) ->
	os_bin_get (erlang:binary_to_list (Key), Default);
	
os_bin_get (Key, Default)
		when is_list (Key) ->
	try
		case os:find_executable (Key) of
			false ->
				apply_default_get (Default);
			EncodedTerm ->
				apply_enforce_get (EncodedTerm, {decode, fun decode_string/1})
		end
	catch throw : Error = {error, _Reason} -> Error end.


apply_default_get ({DefaultAction, DefaultValue}) ->
	case DefaultAction of
		default ->
			{ok, DefaultValue};
		error ->
			throw ({error, DefaultValue})
	end.

apply_enforce_get (EncodedTerm, none) ->
	{ok, EncodedTerm};
	
apply_enforce_get (EncodedTerm, {EnforceAction, EnforceValue}) ->
	case EnforceAction of
		decode when is_function (EnforceValue, 1) ->
			Term = enforce_ok_1 (EnforceValue (EncodedTerm)),
			{ok, Term};
		validate when is_tuple (EnforceValue) ->
			Term = EncodedTerm,
			ok = enforce_ok (validate_term (Term, EnforceValue)),
			{ok, Term};
		validate when is_function (EnforceValue, 1) ->
			Term = EncodedTerm,
			ok = enforce_ok (EnforceValue (Term)),
			{ok, Term}
	end.
