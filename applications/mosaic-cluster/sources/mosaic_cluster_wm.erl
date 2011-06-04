
-module (mosaic_cluster_wm).


-export ([init/1, allowed_methods/2, content_types_provided/2, malformed_request/2, handle_as_html/2, handle_as_json/2, ping/2]).


-dispatch ({[], {console}}).
-dispatch ({["console"], {console}}).

-dispatch ({["cluster", "nodes"], {nodes}}).
-dispatch ({["cluster", "nodes", "self", "activate"], {nodes, self, activate}}).
-dispatch ({["cluster", "nodes", "self", "deactivate"], {nodes, self, deactivate}}).

-dispatch ({["cluster", "ring"], {ring}}).
-dispatch ({["cluster", "ring", "include"], {ring, include}}).
-dispatch ({["cluster", "ring", "exclude"], {ring, exclude}}).
-dispatch ({["cluster", "ring", "reboot"], {ring, reboot}}).


-record (state, {target, arguments}).

-include ("mosaic_cluster_wm.hrl").


init (Target) ->
	{ok, #state{target = Target, arguments = none}}.


ping(Request, State = #state{}) ->
    {pong, Request, State}.


allowed_methods (Request, State = #state{}) ->
	{['GET'], Request, State}.


content_types_provided (Request, State = #state{target = Target}) ->
	{ok, Type} = case Target of
		{console} ->
			{ok, html};
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
		html ->
			{[{"text/html", handle_as_html}], Request, State};
		json ->
			{[{"application/json", handle_as_json}], Request, State}
	end.


malformed_request (Request, State = #state{target = Target, arguments = none}) ->
	Outcome = case Target of
		{console} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{nodes} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{nodes, self, Operation} when ((Operation =:= activate) orelse (Operation =:= deactivate)) ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{ring} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{ring, Operation} when ((Operation =:= include) orelse (Operation =:= exclude)) ->
			case mosaic_webmachine:enforce_request ('GET', [{"node", fun mosaic_webmachine:parse_existing_atom/1}], Request) of
				{ok, false, [Node]} ->
					{ok, false, State#state{arguments = dict:from_list ([{node, Node}])}};
				Error = {error, _Reason} ->
					Error
			end;
		{ring, reboot} ->
			mosaic_webmachine:enforce_request ('GET', [], Request)
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


handle_as_html (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{console} ->
			{ok, html, ?console_html_body}
	end,
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).


handle_as_json (Request, State = #state{target = Target, arguments = Arguments}) ->
	Outcome = case Target of
		{nodes} ->
			Self = erlang:node (),
			Peers = erlang:nodes (),
			Nodes = [Self | Peers],
			{ok, json_struct, [
					{self, mosaic_webmachine:format_atom (Self)},
					{peers, lists:map (fun mosaic_webmachine:format_atom/1, Peers)},
					{nodes, lists:map (fun mosaic_webmachine:format_atom/1, Nodes)}]};
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
					{nodes, lists:map (fun mosaic_webmachine:format_atom/1, Nodes)},
					{partitions, lists:map (
							fun ({Key, Node}) ->
								{struct, [
										{key, mosaic_webmachine:format_integer_identifier (Key)},
										{node, mosaic_webmachine:format_atom (Node)}]}
							end, Partitions)}]};
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
