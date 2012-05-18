
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
	broadcast (Agent, Message, infinity, 6000).

broadcast (Agent, Message, Count, Delay)
		when (is_atom (Agent) orelse is_pid (Agent)),
				((Count =:= infinity) orelse (is_integer (Count) andalso (Count > 0))), is_integer (Delay), (Delay > 0) ->
	gen_server:call (Agent, {mosaic_discovery_agent_tcp, broadcast, Message, Count, Delay}).


-record (state, {qualified_name, configuration, socket, socket_ip, socket_port, broadcasts}).
-record (configuration, {identity, shared_secret, socket_ip, socket_port, events}).
-record (broadcast, {reference, message, count, delay, timer}).


init ({QualifiedName, OriginalConfiguration}) ->
	case configure (OriginalConfiguration) of
		{ok, Configuration = #configuration{events = Events, socket_ip = SocketIp, socket_port = SocketPort}} ->
			SocketOptions = [
					inet, {ip, SocketIp}, {reuseaddr, true},
					{add_membership, {SocketIp, {0, 0, 0, 0}}}, {multicast_loop, true},
					binary, {active, true}],
			case gen_udp:open (SocketPort, SocketOptions) of
				{ok, Socket} ->
					true = erlang:link (Events),
					State = #state{
							qualified_name = QualifiedName, configuration = Configuration,
							socket = Socket, socket_ip = SocketIp, socket_port = SocketPort,
							broadcasts = orddict:new ()},
					{ok, State};
				{error, Reason} ->
					{stop, Reason}
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
		when ((Count =:= infinity) orelse (is_integer (Count) andalso (Count > 0))), is_integer (Delay), (Delay > 0) ->
	Reference = erlang:make_ref (),
	case timer:send_interval (Delay, {mosaic_discovery_agent_tcp_internals, broadcast, Reference}) of
		{ok, Timer} ->
			NewBroadcast = #broadcast{reference = Reference, message = Message, count = Count, delay = Delay, timer = Timer},
			NewBroadcasts = orddict:store (Reference, NewBroadcast, OldBroadcasts),
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
					configuration = #configuration{identity = Identity, shared_secret = SharedSecret},
					socket = Socket, socket_ip = SocketIp, socket_port = SocketPort,
					broadcasts = OldBroadcasts})
		when is_reference (Reference) ->
	case orddict:find (Reference, OldBroadcasts) of
		{ok, OldBroadcast = #broadcast{reference = Reference, message = Message, count = OldCount, timer = Timer}} ->
			{ok, Payload} = encode_payload (Identity, SharedSecret, Message),
			ok = gen_udp:send (Socket, SocketIp, SocketPort, Payload),
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
			{udp, Socket, SourceIp, SourcePort, Payload},
			State = #state{
					configuration = #configuration{identity = Identity, shared_secret = SharedSecret, events = Events},
					socket = Socket}) ->
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
	SocketIp = {224, 0, 0, 1},
	SocketPort = 5556,
	{ok, Events} = mosaic_process_tools:resolve_registered ({local, mosaic_discovery_events}),
	Configuration = #configuration{
			identity = Identity,
			shared_secret = SharedSecret,
			socket_ip = SocketIp,
			socket_port = SocketPort,
			events = Events},
	{ok, Configuration}.
