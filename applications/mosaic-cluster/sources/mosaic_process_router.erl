
-module (mosaic_process_router).

-export ([call/5, cast/4]).


call (Router, Sender = {SenderProcess, SenderToken}, Component, Request, RequestData)
		when (is_pid (Router) orelse is_atom (Router)), is_pid (SenderProcess), is_reference (SenderToken),
				is_binary (Component), (bit_size (Component) =:= 160), is_binary (RequestData) ->
	ok = gen_server:cast (Router, {call, Component, Request, RequestData, Sender}),
	ok.


cast (Router, Component, Request, RequestData)
		when (is_pid (Router) orelse is_atom (Router)),
				is_binary (Component), (bit_size (Component) =:= 160), is_binary (RequestData) ->
	ok = gen_server:cast (Router, {cast, Component, Request, RequestData}),
	ok.
