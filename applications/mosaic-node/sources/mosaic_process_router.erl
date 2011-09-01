
-module (mosaic_process_router).


-export ([resolve/1, resolve/2]).
-export ([register/2, register/3, unregister/2, unregister/3]).
-export ([call/5, call/6, cast/4, cast/5]).


resolve (Identifier) ->
	resolve (mosaic_process_router, Identifier).

resolve (Delegate, Identifier)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	gen_server:call (Delegate, {mosaic_process_router, resolve, Identifier}).


register (Identifier, Process) ->
	register (mosaic_process_router, Identifier, Process).

register (Delegate, Identifier, Process)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	try gen_server:call (Delegate, {mosaic_process_router, register, Identifier, Process}) of
		ok -> ok;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


unregister (Identifier, Process) ->
	unregister (mosaic_process_router, Identifier, Process).

unregister (Delegate, Identifier, Process)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	try gen_server:call (Delegate, {mosaic_process_router, unregister, Identifier, Process}) of
		ok -> ok;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


call (Identifier, Operation, Inputs, Data, Sender) ->
	call (mosaic_process_router, Identifier, Operation, Inputs, Data, Sender).

call (Delegate, Identifier, Operation, Inputs, Data, Sender = {SenderProcess, SenderReference})
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160),
				is_binary (Operation), is_binary (Data), is_pid (SenderProcess), is_reference (SenderReference) ->
	try gen_server:call (Delegate, {mosaic_process_router, call, Identifier, Operation, Inputs, Data, Sender}) of
		ok -> ok;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end;
	
call (Delegate, Identifier, Operation, Inputs, Data, undefined)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160),
				is_binary (Operation), is_binary (Data) ->
	try gen_server:call (Delegate, {mosaic_process_router, call, Identifier, Operation, Inputs, Data}) of
		Outcome = {ok, _Outputs, Data} when is_binary (Data) -> Outcome;
		Error = {error, _Reason, Data} when is_binary (Data) -> Error;
		Error = {error, _Reason} -> Error;
		Reply -> {error, {invalid_reply, Reply}}
	catch exit : {Reason, {gen_server, call, _}} -> {error, Reason} end.


cast (Identifier, Operation, Inputs, Data) ->
	cast (mosaic_process_router, Identifier, Operation, Inputs, Data).

cast (Delegate, Identifier, Operation, Inputs, Data)
		when (is_pid (Delegate) orelse is_atom (Delegate)), is_binary (Identifier), (bit_size (Identifier) =:= 160),
				is_binary (Operation), is_binary (Data) ->
	ok = gen_server:cast (Delegate, {mosaic_process_router, cast, Identifier, Operation, Inputs, Data}),
	ok.
