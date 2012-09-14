
-module (mosaic_component_transcript).


-export ([push/2, push/3, select/0, select/1, select/2]).


push (Identifier, Data) ->
	push (mosaic_component_transcript, Identifier, Data).

push (Delegate, Identifier, Data)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160),
				is_binary (Data) ->
	try gen_server:call (Delegate, {mosaic_component_transcript, push, Identifier, Data}) of
		ok -> ok;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


select () ->
	select (mosaic_component_transcript, undefined).

select (Identifier) ->
	select (mosaic_component_transcript, Identifier).

select (Delegate, Identifier)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	try gen_server:call (Delegate, {mosaic_component_transcript, select, Identifier}) of
		Outcome = {ok, _Records} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end;
	
select (Delegate, undefined)
		when (is_pid (Delegate) orelse is_atom (Delegate)) ->
	try gen_server:call (Delegate, {mosaic_component_transcript, select}) of
		Outcome = {ok, _Records} -> Outcome;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.
