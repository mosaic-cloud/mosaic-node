
-module (mosaic_discovery_agent_tcp).

-behaviour (gen_server).


-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/0, start_supervised/1]).
-export ([stop/0, stop/1, stop/2]).
-export ([broadcast/1, broadcast/2, broadcast/3, broadcast/4]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start () ->
	start (defaults).

start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_process_tools:start (gen_server, mosaic_discovery_agent_tcp, QualifiedName, Configuration).


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_discovery_agent_tcp, QualifiedName, Configuration).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_node_sup:start_child_daemon ({local, mosaic_discovery_agent_tcp}, mosaic_discovery_agent_tcp, Configuration).


stop () ->
	stop (mosaic_discovery_agent_tcp).

stop (Agent) ->
	stop (Agent, normal).

stop (Agent, Signal)
		when (is_atom (Agent) orelse is_pid (Agent)) ->
	gen_server:call (Agent, {mosaic_discovery_agent_tcp, stop, Signal}).


broadcast (Message) ->
	broadcast (mosaic_discovery_agent_tcp, Message).

broadcast (Message, Count, Delay) ->
	broadcast (mosaic_discovery_agent_tcp, Message, Count, Delay).

broadcast (Agent, Message) ->
	broadcast (Agent, Message, infinity, default).

broadcast (Agent, Message, Count, Delay)
		when (is_atom (Agent) orelse is_pid (Agent)),
				((Count =:= infinity) orelse (is_integer (Count) andalso (Count > 0))), ((Delay =:= default) orelse (is_integer (Delay) andalso (Delay > 0))) ->
	gen_server:call (Agent, {mosaic_discovery_agent_tcp, broadcast, Message, Count, Delay}).


-include_lib ("kernel/include/inet.hrl").


-record (state, {qualified_name, configuration, socket, socket_ip, socket_port, broadcasts}).
-record (configuration, {domain, identity, shared_secret, socket_ip, socket_port, events}).
-record (broadcast, {reference, message, count, delay, timer}).


init ({QualifiedName, OriginalConfiguration}) ->
	case configure (OriginalConfiguration) of
		{ok, Configuration = #configuration{events = Events, socket_ip = SocketIp, socket_port = SocketPort}} ->
			SocketOptions = [
					inet, {ip, SocketIp}, {reuseaddr, true},
					binary, {packet, 2}, {active, false}],
			case gen_tcp:listen (SocketPort, SocketOptions) of
				{ok, Socket} ->
					true = erlang:link (Events),
					Self = erlang:self (),
					_Acceptor = proc_lib:spawn_link (fun () -> acceptor_init (Self, Socket) end),
					State = #state{
							qualified_name = QualifiedName, configuration = Configuration,
							socket = Socket, socket_ip = SocketIp, socket_port = SocketPort,
							broadcasts = orddict:new ()},
					{ok, State};
				{error, Reason} ->
					{stop, {failed_socket, Reason}}
			end;
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{socket = Socket}) ->
	case gen_udp:close (Socket) of
		ok ->
			ok;
		{error, _CloseReason} ->
			ok
	end.


code_change (_OldVsn, State, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_discovery_agent_tcp, stop, Signal}, _Sender, State) ->
	case Signal of
		normal ->
			{stop, normal, ok, State};
		_ ->
			{reply, {error, {invalid_signal, Signal}}, State}
	end;
	
handle_call ({mosaic_discovery_agent_tcp, broadcast, Message, Count, Delay}, _Sender, OldState = #state{broadcasts = OldBroadcasts})
		when ((Count =:= infinity) orelse (is_integer (Count) andalso (Count > 0))), ((Delay =:= default) orelse (is_integer (Delay) andalso (Delay > 0))) ->
	{ok, SignalDelay} = case Delay of
		default ->
			case application:get_env (mosaic_node, discovery_agent_tcp_default_broadcast_delay) of
				{ok, DefaultDelay} when is_integer (DefaultDelay), (DefaultDelay > 0) ->
					{ok, DefaultDelay};
				undefined ->
					{error, unconfigured}
			end;
		_ ->
			{ok, Delay}
	end,
	Reference = erlang:make_ref (),
	SignalMessage = {mosaic_discovery_agent_tcp_internals, broadcast, Reference},
	case timer:send_interval (SignalDelay, SignalMessage) of
		{ok, Timer} ->
			NewBroadcast = #broadcast{reference = Reference, message = Message, count = Count, delay = SignalDelay, timer = Timer},
			NewBroadcasts = orddict:store (Reference, NewBroadcast, OldBroadcasts),
			{ok, _} = timer:send_after (1000, SignalMessage),
			{reply, {ok, Reference}, OldState#state{broadcasts = NewBroadcasts}};
		Error = {error, _Reason} ->
			Error
	end;
	
handle_call (Request, _Sender, State) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (
			{mosaic_discovery_agent_tcp_internals, broadcast, Reference},
			OldState = #state{
					configuration = #configuration{domain = Domain, identity = Identity, shared_secret = SharedSecret},
					socket_port = SocketPort,
					broadcasts = OldBroadcasts})
		when is_reference (Reference) ->
	case orddict:find (Reference, OldBroadcasts) of
		{ok, OldBroadcast = #broadcast{reference = Reference, message = Message, count = OldCount, timer = Timer}} ->
			{ok, Payload} = encode_payload (Identity, SharedSecret, Message),
			ok = case send (Domain, SocketPort, Payload) of
				ok ->
					ok;
				{ok, _Sende} ->
					ok
			end,
			case OldCount of
				infinity ->
					{noreply, OldState};
				1 ->
					{ok, cancel} = timer:cancel (Timer),
					NewBroadcasts = orddict:erase (Reference, OldBroadcasts),
					{noreply, OldState#state{broadcasts = NewBroadcasts}};
				_ ->
					NewBroadcasts = orddict:store (Reference, OldBroadcast#broadcast{count = OldCount - 1}, OldBroadcasts),
					{noreply, OldState#state{broadcasts = NewBroadcasts}}
			end;
		error ->
			ok = mosaic_transcript:trace_error ("received invalid broadcast request: invalid reference; ignoring!", [{reference, Reference}]),
			{noreply, OldState}
	end;
	
handle_info (
			{tcp_accept, Socket, ConnectionSocket},
			State = #state{socket = Socket}) ->
	Self = erlang:self (),
	Inputer = proc_lib:spawn_link (fun () -> inputer_init (Self, ConnectionSocket) end),
	case gen_tcp:controlling_process (ConnectionSocket, Inputer) of
		ok ->
			Inputer ! {tcp_connect, ConnectionSocket},
			{noreply, State};
		{error, Reason} ->
			{stop, {error, {failed_socket, ConnectionSocket, Reason}}}
	end;
	
handle_info (
			{tcp, _Socket, SourceIp, SourcePort, Payload},
			State = #state{
					configuration = #configuration{identity = Identity, shared_secret = SharedSecret, events = Events}}) ->
	case decode_payload (Identity, SharedSecret, Payload) of
		ok ->
			{noreply, State};
		{ok, Message} ->
			ok = mosaic_discovery_events:broadcasted (Events, Message),
			{noreply, State};
		{error, Reason} ->
			ok = mosaic_transcript:trace_error ("received invalid broadcast packet: invalid payload; ignoring!", [{source, {SourceIp, SourcePort}}, {payload, Payload}, {reason, Reason}]),
			{noreply, State}
	end;
	
handle_info (Message, State) ->
	{stop, {error, {invalid_message, Message}}, State}.


send ("", _SocketPort, _Payload) ->
	ok;
	
send (Domain, SocketPort, Payload) ->
	Self = erlang:self (),
	Sender = proc_lib:spawn_link (
			fun () ->
				case inet:getaddrs (Domain, inet) of
					{ok, SocketIps} ->
						_Outputers = lists:map (
								fun (SocketIp) ->
									SocketOptions = [
											inet,
											binary, {packet, 2}, {active, false}],
									Outputer = proc_lib:spawn_link (
											fun () ->
												case gen_tcp:connect (SocketIp, SocketPort, SocketOptions) of
													{ok, Socket} ->
														erlang:self () ! {tcp_connect, Socket},
														erlang:self () ! {tcp, Socket, Payload},
														erlang:self () ! {tcp_close, Socket},
														outputer_init (Self, Socket);
													{error, Reason} ->
														ok = mosaic_transcript:trace_warning ("connecting failed; ignoring!", [{target, {SocketIp, SocketPort}}, {reason, Reason}]),
														erlang:exit (normal)
												end
											end),
									Outputer
								end,
								SocketIps),
						erlang:exit (normal);
					{error, Reason} ->
						ok = mosaic_transcript:trace_warning ("resolving failed; ignoring!", [{domain, Domain}, {reason, Reason}]),
						erlang:exit (normal)
				end
			end),
	{ok, Sender}.


acceptor_init (Agent, Socket) ->
	acceptor_loop (Agent, Socket).

acceptor_loop (Agent, Socket) ->
	ok = receive
		{tcp_close, Socket} ->
			case gen_tcp:close (Socket) of
				ok ->
					erlang:exit (normal);
				{error, Reason1} ->
					erlang:exit ({error, {failed_socket, Socket, Reason1}})
			end;
		Message = {system, Sender, Request} ->
			ok = mosaic_transcript:trace_error ("received unexpected system request; aborting!", [{sender, Sender}, {request, Request}]),
			erlang:exit ({error, {invalid_message, Message}});
		Message ->
			ok = mosaic_transcript:trace_error ("received invalid message; aborting!", [{message, Message}]),
			erlang:exit ({error, {invalid_message, Message}})
	after 0 ->
		ok
	end,
	case gen_tcp:accept (Socket, 250) of
		{ok, ConnectionSocket} ->
			case gen_tcp:controlling_process (ConnectionSocket, Agent) of
				ok ->
					Agent ! {tcp_accept, Socket, ConnectionSocket},
					acceptor_loop (Agent, Socket);
				{error, Reason2} ->
					erlang:exit ({error, {failed_socket, ConnectionSocket, Reason2}})
			end;
		{error, timeout} ->
			acceptor_loop (Agent, Socket);
		{error, Reason2} ->
			erlang:exit ({error, {failed_socket, Socket, Reason2}})
	end.


inputer_init (Agent, Socket) ->
	ok = receive
		{tcp_connect, Socket} ->
			ok
	end,
	{SourceIp, SourcePort} = case inet:peername (Socket) of
		{ok, {SourceIp_, SourcePort_}} ->
			{SourceIp_, SourcePort_};
		{error, Reason1} ->
			erlang:exit ({error, {failed_socket, Socket, Reason1}})
	end,
	case gen_tcp:shutdown (Socket, write) of
		ok ->
			inputer_loop (Agent, Socket, SourceIp, SourcePort);
		{error, Reason2} ->
			erlang:exit ({error, {failed_socket, Socket, Reason2}})
	end.

inputer_loop (Agent, Socket, SourceIp, SourcePort) ->
	ok = receive
		{tcp_close, Socket} ->
			case gen_tcp:close (Socket) of
				ok ->
					erlang:exit (normal);
				{error, Reason1} ->
					erlang:exit ({error, {failed_socket, Socket, Reason1}})
			end;
		Message = {system, Sender, Request} ->
			ok = mosaic_transcript:trace_error ("received unexpected system request; aborting!", [{sender, Sender}, {request, Request}]),
			erlang:exit ({error, {invalid_message, Message}});
		Message ->
			ok = mosaic_transcript:trace_error ("received invalid message; aborting!", [{message, Message}]),
			erlang:exit ({error, {invalid_message, Message}})
	after 0 ->
		ok
	end,
	case gen_tcp:recv (Socket, 0, 250) of
		{ok, Payload} ->
			Agent ! {tcp, Socket, SourceIp, SourcePort, Payload},
			inputer_loop (Agent, Socket, SourceIp, SourcePort);
		{error, timeout} ->
			inputer_loop (Agent, Socket, SourceIp, SourcePort);
		{error, closed} ->
			erlang:exit (normal);
		{error, Reason2} ->
			erlang:exit ({error, {failed_socket, Socket, Reason2}})
	end.


outputer_init (Agent, Socket) ->
	ok = receive
		{tcp_connect, Socket} ->
			ok
	end,
	{TargetIp, TargetPort} = case inet:peername (Socket) of
		{ok, {TargetIp_, TargetPort_}} ->
			{TargetIp_, TargetPort_};
		{error, Reason1} ->
			erlang:exit ({error, {failed_socket, Socket, Reason1}})
	end,
	case gen_tcp:shutdown (Socket, read) of
		ok ->
			outputer_loop (Agent, Socket, TargetIp, TargetPort);
		{error, Reason} ->
			erlang:exit ({error, {failed_socket, Socket, Reason}})
	end.

outputer_loop (Agent, Socket, TargetIp, TargetPort) ->
	receive
		{tcp, Socket, Payload} when is_binary (Payload) ->
			case gen_tcp:send (Socket, Payload) of
				ok ->
					outputer_loop (Agent, Socket, TargetIp, TargetPort);
				{error, closed} ->
					ok = mosaic_transcript:trace_warning ("connection closed; ignoring!", [{target, {TargetIp, TargetPort}}]),
					erlang:exit (normal);
				{error, Reason} ->
					erlang:exit ({error, {failed_socket, Socket, Reason}})
			end;
		{tcp_close, Socket} ->
			case gen_tcp:close (Socket) of
				ok ->
					erlang:exit (normal);
				{error, Reason} ->
					erlang:exit ({error, {failed_socket, Socket, Reason}})
			end;
		Message = {system, Sender, Request} ->
			ok = mosaic_transcript:trace_error ("received unexpected system request; aborting!", [{sender, Sender}, {request, Request}]),
			erlang:exit ({error, {invalid_message, Message}});
		Message ->
			ok = mosaic_transcript:trace_error ("received invalid message; aborting!", [{message, Message}]),
			erlang:exit ({error, {invalid_message, Message}})
	end.


encode_payload (Identity, SharedSecret, Message)
		when is_binary (Identity), (bit_size (Identity) =:= 128), is_binary (SharedSecret), (bit_size (SharedSecret) =:= 128) ->
	MessageData = erlang:term_to_binary ({os:timestamp (), Message}),
	PayloadData = <<Identity / binary, MessageData / binary>>,
	PayloadMac = crypto:md5_mac (SharedSecret, PayloadData),
	Payload = <<PayloadMac / binary, PayloadData / binary>>,
	{ok, Payload}.


decode_payload (Identity, SharedSecret, Payload)
		when is_binary (SharedSecret), (bit_size (SharedSecret) =:= 128), is_binary (Identity), (bit_size (Identity) =:= 128),
				is_binary (Payload) ->
	PayloadSize = bit_size (Payload),
	if
		(PayloadSize > 256) ->
			case Payload of
				<<_PayloadMac : 128 / bits, Identity : 128 / bits, _MessageData / binary>> ->
					ok;
				<<PayloadMac : 128 / bits, PayloadData / binary >> = Payload ->
					case crypto:md5_mac (SharedSecret, PayloadData) of
						PayloadMac ->
							<<_Identity : 128 / bits, MessageData / binary>> = PayloadData,
							try
								case erlang:binary_to_term (MessageData) of
									{{Time1, Time2, Time3}, Message}
											when is_integer (Time1), is_integer (Time2), is_integer (Time3),
													(Time1 > 0), (Time2 > 0), (Time3 > 0) ->
										{ok, Message};
									PayloadTerm ->
										{error, {invalid_payload_term, PayloadTerm}}
								end
							catch
								error : badarg ->
									{error, {invalid_payload_data, PayloadData}}
							end;
						OtherPayloadMac ->
							{error, {invalid_payload_mac, OtherPayloadMac, PayloadMac}}
					end
			end;
		true ->
			{error, {invalid_payload_size, PayloadSize}}
	end.


configure (defaults) ->
	Identity = crypto:rand_bytes (16),
	SharedSecret = case erlang:get_cookie () of
		nocookie ->
			crypto:rand_bytes (16);
		Cookie ->
			crypto:md5 (erlang:atom_to_binary (Cookie, utf8))
	end,
	case application:get_env (mosaic_node, discovery_agent_tcp_address) of
		{ok, {Domain, SocketAddress, SocketPort}} when is_list (Domain), is_list (SocketAddress), is_integer (SocketPort), (SocketPort >= 0), (SocketPort < 65536) ->
			case inet:gethostbyname (SocketAddress, inet) of
				{ok, #hostent{h_addrtype = inet, h_addr_list = [SocketIp | _]}} ->
					{ok, Events} = mosaic_process_tools:resolve_registered ({local, mosaic_discovery_events}),
					Configuration = #configuration{
							domain = Domain,
							identity = Identity,
							shared_secret = SharedSecret,
							socket_ip = SocketIp,
							socket_port = SocketPort,
							events = Events},
					{ok, Configuration};
				Error = {error, _Reason} ->
					Error
			end;
		{ok, Configuration} ->
			{error, {invalid_configuration, Configuration}};
		undefined ->
			{error, unconfigured}
	end.
