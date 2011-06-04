
-module (mosaic_process_router).


-export ([resolve/1, resolve/2]).
-export ([register/2, register/3, unregister/2, unregister/3]).
-export ([call/4, call/5, cast/3, cast/4]).


resolve (Identifier) ->
	resolve (mosaic_process_router, Identifier).

resolve (Router, Identifier)
		when (is_pid (Router) orelse is_atom (Router)), is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	gen_server:call (Router, {mosaic_process_router, resolve, Identifier}).


register (Identifier, Process) ->
	register (mosaic_process_router, Identifier, Process).

register (Router, Identifier, Process)
		when (is_pid (Router) orelse is_atom (Router)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	gen_server:call (Router, {mosaic_process_router, register, Identifier, Process}).


unregister (Identifier, Process) ->
	unregister (mosaic_process_router, Identifier, Process).

unregister (Router, Identifier, Process)
		when (is_pid (Router) orelse is_atom (Router)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	gen_server:call (Router, {mosaic_process_router, unregister, Identifier, Process}).


call (Identifier, Request, RequestData, Sender) ->
	call (mosaic_process_router, Identifier, Request, RequestData, Sender).

call (Router, Identifier, Request, RequestData, Sender = {SenderProcess, SenderReference})
		when (is_pid (Router) orelse is_atom (Router)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (RequestData),
				is_pid (SenderProcess), is_reference (SenderReference) ->
	gen_server:cast (Router, {mosaic_process_router, call, Identifier, Request, RequestData, Sender});
	
call (Router, Identifier, Request, RequestData, undefined)
		when (is_pid (Router) orelse is_atom (Router)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (RequestData) ->
	gen_server:call (Router, {mosaic_process_router, call, Identifier, Request, RequestData}).


cast (Identifier, Request, RequestData) ->
	cast (mosaic_process_router, Identifier, Request, RequestData).

cast (Router, Identifier, Request, RequestData)
		when (is_pid (Router) orelse is_atom (Router)), is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (RequestData) ->
	gen_server:cast (Router, {mosaic_process_router, cast, Identifier, Request, RequestData}).
