
-module (mosaic_component_coders).


-export ([
		validate_component/1, encode_component/1, decode_component/1, generate_component/0,
		validate_group/1, encode_group/1, decode_group/1, generate_group/0,
		validate_correlation/1, encode_correlation/1, decode_correlation/1, generate_correlation/0]).
-export ([
		validate_resource_specifications/1, validate_resource_specification/1,
		validate_resource_descriptors/1, validate_resource_descriptor/1,
		encode_resource_specifications/2, decode_resource_specifications/2,
		encode_resource_specification/2, decode_resource_specification/2,
		encode_resource_descriptors/2, decode_resource_descriptors/2,
		encode_resource_descriptor/2, decode_resource_descriptor/2]).
-export ([
		encode_packet/1, decode_packet/1,
		encode_packet_fully/1, decode_packet_fully/1]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


validate_component (Component) ->
	mosaic_generic_coders:validate_term (Component, {is_bitstring, 160, invalid_component}).

encode_component (Component)
		when is_binary (Component), (bit_size (Component) =:= 160) ->
	mosaic_generic_coders:encode_hex_data (Component).

decode_component (EncodedComponent)
		when is_binary (EncodedComponent); is_list (EncodedComponent) ->
	try
		case enforce_ok_1 (mosaic_generic_coders:decode_hex_data (EncodedComponent)) of
			Component when (bit_size (Component) =:= 160) ->
				{ok, Component};
			_ ->
				throw ({error, invalid_length})
		end
	catch throw : {error, Reason} -> {error, {invalid_component, EncodedComponent, Reason}} end.

generate_component () ->
	mosaic_generic_coders:generate_data (160 div 8).


validate_group (Group) ->
	mosaic_generic_coders:validate_term (Group, {is_bitstring, 160, invalid_group}).

encode_group (Group)
		when is_binary (Group) ->
	encode_component (Group).

decode_group (EncodedGroup)
		when is_binary (EncodedGroup); is_list (EncodedGroup) ->
	decode_component (EncodedGroup).

generate_group () ->
	mosaic_generic_coders:generate_data (160 div 8).


validate_correlation (Correlation) ->
	mosaic_generic_coders:validate_term (Correlation, {is_bitstring, 128, invalid_correlation}).

encode_correlation (Correlation)
		when is_binary (Correlation), (bit_size (Correlation) =:= 128) ->
	mosaic_generic_coders:encode_hex_data (Correlation).

decode_correlation (EncodedCorrelation)
		when is_binary (EncodedCorrelation); is_list (EncodedCorrelation) ->
	try
		case enforce_ok_1 (mosaic_generic_coders:decode_hex_data (EncodedCorrelation)) of
			Correlation when (bit_size (Correlation) =:= 128) ->
				{ok, Correlation};
			_ ->
				throw ({error, invalid_length})
		end
	catch trow : {error, Reason} -> {error, {invalid_correlation, EncodedCorrelation, Reason}} end.

generate_correlation () ->
	mosaic_generic_coders:generate_data (128 div 8).


validate_resource_specifications (Specifications) ->
	mosaic_generic_coders:validate_term (Specifications, {is_list, {validator, fun validate_resource_specification/1}, invalid_specifications}).

validate_resource_specification (Specification) ->
	mosaic_generic_coders:validate_term (Specification,
				{is_tuple, {{is_binary, invalid_identifier}, {is_binary, invalid_type}, {matches, defaults}}, invalid_specification}).

encode_resource_specifications (json, Specifications)
		when is_list (Specifications), (Specifications =/= []) ->
	try
		{ok, {struct, lists:map (
					fun (Specification) ->
						enforce_ok_1 (encode_resource_specification (json, Specification))
					end, Specifications)}}
	catch throw : {error, Reason} -> {error, {invalid_specifications, Specifications, Reason}} end;
	
encode_resource_specifications (json, Specifications) ->
	{error, {invalid_specifications, Specifications}}.

encode_resource_specification (json, {Identifier, Type})
		when is_binary (Identifier), is_binary (Type) ->
	{ok, {Identifier, Type}};
	
encode_resource_specification (json, Specification) ->
	{error, {invalid_specification, Specification}}.

decode_resource_specifications (json, {struct, Specifications})
		when is_list (Specifications), (Specifications =/= []) ->
	try
		{ok, lists:map (
					fun (Specification) ->
						enforce_ok_1 (decode_resource_specification (json, Specification))
					end, Specifications)}
	catch throw : {error, Reason} -> {error, {invalid_specifications, Specifications, Reason}} end;
	
decode_resource_specifications (json, Specifications) ->
	{error, {invalid_specifications, Specifications}}.

decode_resource_specification (json, {Identifier, Type})
		when is_binary (Identifier), is_binary (Type) ->
	{ok, {Identifier, Type, defaults}};
	
decode_resource_specification (json, Specification) ->
	{error, {invalid_specification, Specification}}.


validate_resource_descriptors (Descriptors) ->
	mosaic_generic_coders:validate_term (Descriptors, {is_list, {validator, fun validate_resource_descriptor/1}, invalid_descriptors}).

validate_resource_descriptor (Descriptor) ->
	mosaic_generic_coders:validate_term (Descriptor,
				{is_tuple, {{is_binary, invalid_identifier}, {is_list, {validator, fun validate_resource_descriptor_attribute/1}, invalid_attributes}}, invalid_descriptor}).

validate_resource_descriptor_attribute (Attribute) ->
	mosaic_generic_coders:validate_term (Attribute,
				{is_tuple, {{is_binary, invalid_name}, {'orelse', [is_binary, is_integer, is_float, is_boolean], invalid_value}}, invalid_attribute}).

encode_resource_descriptors (json, Descriptors)
		when is_list (Descriptors), (Descriptors =/= []) ->
	try
		{ok, {struct, lists:map (
					fun (Descriptor) ->
						enforce_ok_1 (encode_resource_descriptor (json, Descriptor))
					end, Descriptors)}}
	catch throw : {error, Reason} -> {error, {invalid_descriptors, Descriptors, Reason}} end;
	
encode_resource_descriptors (json, Descriptors) ->
	{error, {invalid_descriptors, Descriptors}}.

encode_resource_descriptor (json, Descriptor = {Identifier, Attributes})
		when is_binary (Identifier), is_list (Attributes), (Attributes =/= []) ->
	try
		{ok, {
					Identifier,
					lists:map (
								fun (Attribute) ->
									enforce_ok_1 (encode_resource_descriptor_attribute (json, Attribute))
								end, Attributes)}}
	catch throw : {error, Reason} -> {error, {invalid_descriptor, Descriptor, Reason}} end;
	
encode_resource_descriptor (json, Descriptor) ->
	{error, {invalid_descriptor, Descriptor}}.

encode_resource_descriptor_attribute (json, Attribute = {Name, Value})
		when is_binary (Name) ->
	try
		EncodedValue = if
			is_binary (Value); is_integer (Value); is_float (Value); is_boolean (Value); (Value =:= null) ->
				Value;
			true ->
				throw ({error, invalid_value})
		end,
		{ok, {Name, EncodedValue}}
	catch throw : {error, Reason} -> {error, {invalid_attribute, Attribute, Reason}} end;
	
encode_resource_descriptor_attribute (json, Attribute) ->
	{error, {invalid_attribute, Attribute}}.

decode_resource_descriptors (json, {struct, Descriptors})
		when is_list (Descriptors), (Descriptors =/= []) ->
	try
		{ok, lists:map (
					fun (Descriptor) ->
						enforce_ok_1 (decode_resource_descriptor (json, Descriptor))
					end, Descriptors)}
	catch throw : {error, Reason} -> {error, {invalid_descriptors, Descriptors, Reason}} end;
	
decode_resource_descriptors (json, Descriptors) ->
	{error, {invalid_descriptors, Descriptors}}.

decode_resource_descriptor (json, Descriptor = {Identifier, {struct, Attributes}})
		when is_binary (Identifier), is_list (Attributes) ->
	try
		{ok, {
					Identifier,
					lists:map (
							fun (Attribute) ->
								enforce_ok_1 (decode_resource_descriptor_attribute (json, Attribute))
							end, Attributes)}}
	catch throw : {error, Reason} -> {error, {invalid_descriptor, Descriptor, Reason}} end;
	
decode_resource_descriptor (json, Descriptor) ->
	{error, {invalid_descriptor, Descriptor}}.

decode_resource_descriptor_attribute (json, Attribute = {Name, EncodedValue})
		when is_binary (Name) ->
	try
		Value = if
			is_binary (EncodedValue); is_integer (EncodedValue); is_float (EncodedValue); is_boolean (EncodedValue); (EncodedValue =:= null) ->
				EncodedValue;
			true ->
				throw ({error, invalid_value})
		end,
		{ok, {Name, Value}}
	catch throw : {error, Reason} -> {error, {invalid_attribute, Attribute, Reason}} end;
	
decode_resource_descriptor_attribute (json, Attribute) ->
	{error, {invalid_attribute, Attribute}}.


encode_packet ({call, Component, Operation, Correlation, Inputs, Data})
		when is_binary (Component), is_binary (Operation), is_binary (Correlation), is_binary (Data) ->
	try
		EncodedComponent = enforce_ok_1 (encode_component (Component)),
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {exchange, [
					{<<"action">>, <<"call">>}, {<<"component">>, EncodedComponent}, {<<"operation">>, Operation},
					{<<"correlation">>, EncodedCorrelation}, {<<"inputs">>, Inputs}], Data}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({call, Operation, Correlation, Inputs, Data})
		when is_binary (Operation), is_binary (Correlation), is_binary (Data) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {exchange, [
					{<<"action">>, <<"call">>}, {<<"operation">>, Operation},
					{<<"correlation">>, EncodedCorrelation}, {<<"inputs">>, Inputs}], Data}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({call_return, Correlation, {ok, Outputs, Data}})
		when is_binary (Correlation), is_binary (Data) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {exchange, [
					{<<"action">>, <<"call-return">>}, {<<"correlation">>, EncodedCorrelation},
					{<<"ok">>, true}, {<<"outputs">>, Outputs}], Data}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({call_return, Correlation, {error, Reason, Data}})
		when is_binary (Correlation), is_binary (Data) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {exchange, [
					{<<"action">>, <<"call-return">>}, {<<"correlation">>, EncodedCorrelation},
					{<<"ok">>, false}, {<<"error">>, Reason}], Data}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({call_return, Correlation, {error, Reason}})
		when is_binary (Correlation) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		EncodedReason = enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason)),
		{ok, {exchange, [
					{<<"action">>, <<"call-return">>}, {<<"correlation">>, EncodedCorrelation},
					{<<"ok">>, false}, {<<"error">>, EncodedReason}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({cast, Component, Operation, Inputs, Data})
		when is_binary (Component), is_binary (Operation), is_binary (Data) ->
	try
		EncodedComponent = enforce_ok_1 (encode_component (Component)),
		{ok, {exchange, [
					{<<"action">>, <<"cast">>}, {<<"component">>, EncodedComponent}, {<<"operation">>, Operation},
					{<<"inputs">>, Inputs}], Data}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({cast, Operation, Inputs, Data})
		when is_binary (Operation), is_binary (Data) ->
	try
		{ok, {exchange, [
					{<<"action">>, <<"cast">>}, {<<"operation">>, Operation},
					{<<"inputs">>, Inputs}], Data}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({register, Group, Correlation})
		when is_binary (Group), is_binary (Correlation) ->
	try
		EncodedGroup = enforce_ok_1 (encode_group (Group)),
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {exchange, [
					{<<"action">>, <<"register">>}, {<<"group">>, EncodedGroup}, {<<"correlation">>, EncodedCorrelation}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({register_return, Correlation, ok})
		when is_binary (Correlation) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {exchange, [
					{<<"action">>, <<"register-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"ok">>, true}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({register_return, Correlation, {error, Reason}})
		when is_binary (Correlation) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		EncodedReason = enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason)),
		{ok, {exchange, [
					{<<"action">>, <<"register-return">>}, {<<"correlation">>, EncodedCorrelation},
					{<<"ok">>, false}, {<<"error">>, EncodedReason}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({acquire, Specifications, Correlation})
		when is_list (Specifications), is_binary (Correlation), (Specifications =/= []) ->
	try
		EncodedSpecifications = enforce_ok_1 (encode_resource_specifications (json, Specifications)),
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		{ok, {resources, [
					{<<"action">>, <<"acquire">>}, {<<"specifications">>, EncodedSpecifications},
					{<<"correlation">>, EncodedCorrelation}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({acquire_return,  Correlation, {ok, Descriptors}})
		when is_binary (Correlation), is_list (Descriptors), (Descriptors =/= []) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		EncodedDescriptors = enforce_ok_1 (encode_resource_descriptors (json, Descriptors)),
		{ok, {resources, [
					{<<"action">>, <<"acquire-return">>}, {<<"correlation">>, EncodedCorrelation},
					{<<"ok">>, true}, {<<"descriptors">>, EncodedDescriptors}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet ({acquire_return, Correlation, {error, Reason}})
		when is_binary (Correlation) ->
	try
		EncodedCorrelation = enforce_ok_1 (encode_correlation (Correlation)),
		EncodedReason = enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason)),
		{ok, {resources, [
					{<<"action">>, <<"acquire-return">>}, {<<"correlation">>, EncodedCorrelation},
					{<<"ok">>, false}, {<<"error">>, EncodedReason}], <<>>}}
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet (Packet) ->
	{error, {invalid_packet, Packet}}.


decode_packet (Packet = {exchange, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	try
		case MetaData of
			[{<<"action">>, <<"call">>}, {<<"component">>, EncodedComponent}, {<<"correlation">>, EncodedCorrelation},
						{<<"inputs">>, Inputs}, {<<"operation">>, Operation}]
					when is_binary (EncodedComponent), is_binary (Operation), is_binary (EncodedCorrelation) ->
				Component = enforce_ok_1 (decode_component (EncodedComponent)),
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {call, Component, Operation, Correlation, Inputs, Data}};
			[{<<"action">>, <<"call">>}, {<<"correlation">>, EncodedCorrelation},
						{<<"inputs">>, Inputs}, {<<"operation">>, Operation}]
					when is_binary (Operation), is_binary (EncodedCorrelation) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {call, Operation, Correlation, Inputs, Data}};
			[{<<"action">>, <<"call-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"ok">>, true}, {<<"outputs">>, Outputs}]
					when is_binary (EncodedCorrelation) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {call_return, Correlation, {ok, Outputs, Data}}};
			[{<<"action">>, <<"call-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"error">>, Reason}, {<<"ok">>, false}]
					when is_binary (EncodedCorrelation) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {call_return, Correlation, {error, Reason, Data}}};
			[{<<"action">>, <<"cast">>}, {<<"component">>, EncodedComponent}, {<<"inputs">>, Inputs}, {<<"operation">>, Operation}]
					when is_binary (EncodedComponent), is_binary (Operation) ->
				Component = enforce_ok_1 (decode_component (EncodedComponent)),
				{ok, {cast, Component, Operation, Inputs, Data}};
			[{<<"action">>, <<"cast">>}, {<<"inputs">>, Inputs}, {<<"operation">>, Operation}]
					when is_binary (Operation) ->
				{ok, {cast, Operation, Inputs, Data}};
			[{<<"action">>, <<"register">>}, {<<"correlation">>, EncodedCorrelation}, {<<"group">>, EncodedGroup}]
					when is_binary (EncodedGroup), is_binary (EncodedCorrelation), (Data =:= <<>>) ->
				Group = enforce_ok_1 (decode_group (EncodedGroup)),
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {register, Group, Correlation}};
			[{<<"action">>, <<"register-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"ok">>, true}]
					when is_binary (EncodedCorrelation), (Data =:= <<>>) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {register_return, Correlation, ok}};
			[{<<"action">>, <<"register-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"error">>, Reason}, {<<"ok">>, false}]
					when is_binary (EncodedCorrelation), (Data =:= <<>>) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {register_return, Correlation, {error, Reason}}};
			_ ->
				throw ({error, {invalid_packet, Packet}})
		end
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet (Packet = {resources, MetaData, Data})
		when is_list (MetaData), is_binary (Data) ->
	try
		case MetaData of
			[{<<"action">>, <<"acquire">>}, {<<"correlation">>, EncodedCorrelation}, {<<"specifications">>, EncodedSpecifications}]
					when is_binary (EncodedCorrelation), (Data =:= <<>>) ->
				Specifications = enforce_ok_1 (decode_resource_specifications (json, EncodedSpecifications)),
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {acquire, Specifications, Correlation}};
			[{<<"action">>, <<"acquire-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"descriptors">>, EncodedDescriptors},
						{<<"ok">>, true}]
					when is_binary (EncodedCorrelation), (Data =:= <<>>) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				Descriptors = enforce_ok_1 (decode_resource_descriptors (json, EncodedDescriptors)),
				{ok, {acquire_return, Correlation, {ok, Descriptors}}};
			[{<<"action">>, <<"acquire-return">>}, {<<"correlation">>, EncodedCorrelation}, {<<"error">>, Reason}, {<<"ok">>, false}]
					when is_binary (EncodedCorrelation), (Data =:= <<>>) ->
				Correlation = enforce_ok_1 (decode_correlation (EncodedCorrelation)),
				{ok, {acquire_return, Correlation, {error, Reason}}};
			_ ->
				throw ({error, {invalid_packet, Packet}})
		end
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet (Packet) ->
	{error, {invalid_packet, Packet}}.


encode_packet_fully (Packet = {exchange, _MetaData, _Data}) ->
	{ok, Packet};
	
encode_packet_fully (Packet = {resources, _MetaData, _Data}) ->
	{ok, Packet};
	
encode_packet_fully (Packet = {generic, _MetaData, _Data}) ->
	{ok, Packet};
	
encode_packet_fully (Packet = {call, _Component, _Operation, _Correlation, _Inputs, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {call, _Operation, _Correlation, _Inputs, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {call_return, _Correlation, {ok, _Outputs, _Data}}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {call_return, _Correlation, {error, _Reason, _Data}}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {call_return, _Correlation, {error, _Reason}}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {cast, _Component, _Operation, _Inputs, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {cast, _Operation, _Inputs, _Data}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {register, _Group, _Correlation}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {register_return, _Correlation, ok}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {register_return, _Correlation, {error, _Reason}}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {acquire, _Correlation, _Specifications}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {acquire_return,  _Correlation, {ok, _Descriptors}}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet = {acquire_return, _Correlation, {error, _Reason}}) ->
	try enforce_ok_1 (encode_packet (Packet)) of
		NewPacket -> encode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
encode_packet_fully (Packet) ->
	{error, {invalid_packet, Packet}}.


decode_packet_fully (Packet = {call, _Component, _Operation, _Correlation, _Inputs, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {call, _Operation, _Correlation, _Inputs, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {call_return, _Correlation, {ok, _Outputs, _Data}}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {call_return, _Correlation, {error, _Reason, _Data}}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {call_return, _Correlation, {error, _Reason}}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {cast, _Component, _Operation, _Inputs, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {cast, _Operation, _Inputs, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {register, _Group, _Correlation}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {register_return, _Correlation, ok}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {register_return, _Correlation, {error, _Reason}}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {acquire, _Correlation, _Specifications}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {acquire_return,  _Correlation, {ok, _Descriptors}}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {acquire_return, _Correlation, {error, _Reason}}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {exit, _ExitStatus}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {generic, _MetaData, _Data}) ->
	{ok, Packet};
	
decode_packet_fully (Packet = {exchange, _MetaData, _Data}) ->
	try enforce_ok_1 (decode_packet (Packet)) of
		NewPacket -> decode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet_fully (Packet = {resources, _MetaData, _Data}) ->
	try enforce_ok_1 (decode_packet (Packet)) of
		NewPacket -> decode_packet_fully (NewPacket)
	catch throw : Error = {error, _Reason} -> Error end;
	
decode_packet_fully (Packet) ->
	{error, {invalid_packet, Packet}}.
