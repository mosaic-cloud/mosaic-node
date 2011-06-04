
-module (mosaic_cluster_processes_router).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_sup:start_child_daemon (mosaic_cluster_processes_router, {local, mosaic_process_router}, [Configuration], permanent).

start_link (QualifiedName, Configuration) ->
	mosaic_tools:start_link (gen_server, mosaic_cluster_processes_router, QualifiedName, Configuration).


-record (state, {qualified_name}).


init ({QualifiedName, defaults}) ->
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			State = #state{qualified_name = QualifiedName},
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{}) ->
	ok.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_process_router, call, Identifier, RequestMetaData, RequestData}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (RequestMetaData) ->
	{reply, {error, unsupported_request}, State};
	
handle_call ({mosaic_process_router, resolve, Identifier}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	{reply, {error, unsupported_request}, State};
	
handle_call ({mosaic_process_router, register, Identifier, Process}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	{reply, {error, unsupported_request}, State};
	
handle_call ({mosaic_process_router, unregister, Identifier, Process}, _Sender, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_pid (Process) ->
	{reply, {error, unsupported_request}, State};
	
handle_call (Request, Sender, State = #state{}) ->
	ok = mosaic_tools:trace_error ("received invalid call request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast ({mosaic_process_router, call, Identifier, RequestMetaData, RequestData, Sender = {SenderProcess, SenderReference}}, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (RequestMetaData),
				is_pid (SenderProcess), is_reference (SenderReference) ->
	{noreply, State};
	
handle_cast ({mosaic_process_router, cast, Identifier, RequestMetaData, RequestData}, State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (RequestMetaData) ->
	{noreply, State};
	
handle_cast (Request, State = #state{}) ->
	ok = mosaic_tools:trace_error ("received invalid cast request; ignoring!", [{request, Request}]),
	{noreply, State}.


handle_info (Message, State = #state{}) ->
	ok = mosaic_tools:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{noreply, State}.
