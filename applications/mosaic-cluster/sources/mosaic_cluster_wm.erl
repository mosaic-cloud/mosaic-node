
-module (mosaic_cluster_wm).


-export ([
		init/1, ping/2,
		allowed_methods/2,
		resource_exists/2, previously_existed/2, moved_temporarily/2,
		malformed_request/2,
		content_types_provided/2,
		handle_as_json/2, handle_static/2]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


-dispatch ({[], {root}}).
-dispatch ({["static", '*'], {static}}).

-dispatch ({["cluster", "nodes"], {nodes}}).
-dispatch ({["cluster", "nodes", "self", "activate"], {nodes, self, activate}}).
-dispatch ({["cluster", "nodes", "self", "deactivate"], {nodes, self, deactivate}}).

-dispatch ({["cluster", "ring"], {ring}}).
-dispatch ({["cluster", "ring", "include"], {ring, include}}).
-dispatch ({["cluster", "ring", "exclude"], {ring, exclude}}).
-dispatch ({["cluster", "ring", "reboot"], {ring, reboot}}).


-record (state, {target, arguments}).


init (Target) ->
	{{trace, "/tmp/mosaic-cluster-webmachine"}, #state{target = Target, arguments = none}}.


ping(Request, State = #state{}) ->
    {pong, Request, State}.


allowed_methods (Request, State = #state{}) ->
	{['GET'], Request, State}.


resource_exists (Request, State = #state{target = Target}) ->
	case Target of
		{root} ->
			{false, Request, State};
		_ ->
			{true, Request, State}
	end.


previously_existed (Request, State = #state{target = Target}) ->
	case Target of
		{root} ->
			{true, Request, State};
		_ ->
			{true, Request, State}
	end.


moved_temporarily (Request, State = #state{target = Target}) ->
	case Target of
		{root} ->
			{{true, "/static/console.html"}, Request, State};
		_ ->
			{false, Request, State}
	end.


malformed_request (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{root} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{static} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{nodes} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{nodes, self, Operation} when ((Operation =:= activate) orelse (Operation =:= deactivate)) ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{ring} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{ring, Operation} when ((Operation =:= include) orelse (Operation =:= exclude)) ->
			case mosaic_webmachine:enforce_request ('GET', [{"node", fun mosaic_generic_coders:decode_atom/1}], Request) of
				{ok, false, [Node]} ->
					{ok, false, State#state{arguments = dict:from_list ([{node, Node}])}};
				Error = {error, _Reason} ->
					Error
			end;
		{ring, reboot} ->
			mosaic_webmachine:enforce_request ('GET', [], Request)
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


content_types_provided (Request, State = #state{target = {root}}) ->
	{[{"application/octet-stream", handle_empty}], Request, State};
	
content_types_provided (Request, State = #state{target = {static}}) ->
	Path = erlang:list_to_binary (lists:map (fun (PathToken) -> [$/, PathToken] end, wrq:path_tokens (Request))),
	case mosaic_static_resources:contents (Path) of
		{ok, MimeType, Data} ->
			{[{erlang:binary_to_list (MimeType), handle_static}], Request, State#state{arguments = {Path, MimeType, Data}}};
		{error, _Reason} ->
			{[], Request, State}
	end;
	
content_types_provided (Request, State = #state{target = Target}) ->
	{ok, Type} = case Target of
		{nodes} ->
			{ok, json};
		{nodes, _, _} ->
			{ok, json};
		{ring} ->
			{ok, json};
		{ring, _} ->
			{ok, json}
	end,
	case Type of
		json ->
			{[{"application/json", handle_as_json}], Request, State}
	end.


handle_as_json (Request, State = #state{target = Target, arguments = Arguments}) ->
	Outcome = case Target of
		{nodes} ->
			Self = erlang:node (),
			Peers = erlang:nodes (),
			Nodes = [Self | Peers],
			{ok, json_struct, [
					{self, enforce_ok_1 (mosaic_generic_coders:encode_atom (Self))},
					{peers, [enforce_ok_1 (mosaic_generic_coders:encode_atom (Peer)) || Peer <- Peers]},
					{nodes, [enforce_ok_1 (mosaic_generic_coders:encode_atom (Node)) || Node <- Nodes]}]};
		{nodes, self, activate} ->
			case mosaic_cluster_tools:node_activate () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{nodes, self, deactivate} ->
			case mosaic_cluster_tools:node_deactivate () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{ring} ->
			{ok, Nodes} = mosaic_cluster_tools:ring_nodes (),
			{ok, Partitions} = mosaic_cluster_tools:ring_partitions (),
			{ok, json_struct, [
					{nodes, [enforce_ok_1 (mosaic_generic_coders:encode_atom (Node)) || Node <- Nodes]},
					{partitions, [
								{struct, [
										{key, enforce_ok_1 (mosaic_generic_coders:encode_hex_data (<<Partition : 160>>))},
										{node, enforce_ok_1 (mosaic_generic_coders:encode_atom (Node))}]}
							|| {Partition, Node} <- Partitions]}]};
		{ring, include} ->
			Node = dict:fetch (node, Arguments),
			case mosaic_cluster_tools:ring_include (Node) of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{ring, exclude} ->
			Node = dict:fetch (node, Arguments),
			case mosaic_cluster_tools:ring_exclude (Node) of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{ring, reboot} ->
			case mosaic_cluster_tools:ring_reboot () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end
	end,
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).


handle_static (Request, State = #state{target = {static}, arguments = {_Path, MimeType, Data}}) ->
	Outcome = {ok, {mime, MimeType}, Data},
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).
