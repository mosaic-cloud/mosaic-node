
-module (mosaic_cluster_wm).


-export ([
		init/1, ping/2,
		allowed_methods/2,
		malformed_request/2,
		content_types_provided/2,
		handle_as_json/2]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


-dispatch ({[<<"v1">>, <<"cluster">>, <<"nodes">>], {nodes}}).
-dispatch ({[<<"v1">>, <<"cluster">>, <<"nodes">>, <<"self">>, <<"activate">>], {nodes, self, activate}}).
-dispatch ({[<<"v1">>, <<"cluster">>, <<"nodes">>, <<"self">>, <<"deactivate">>], {nodes, self, deactivate}}).
-dispatch ({[<<"v1">>, <<"cluster">>, <<"ring">>], {ring}}).
-dispatch ({[<<"v1">>, <<"cluster">>, <<"ring">>, <<"include">>], {ring, include}}).
-dispatch ({[<<"v1">>, <<"cluster">>, <<"ring">>, <<"exclude">>], {ring, exclude}}).
-dispatch ({[<<"v1">>, <<"cluster">>, <<"ring">>, <<"reboot">>], {ring, reboot}}).


-record (state, {target, arguments}).


init (Target) ->
	{ok, #state{target = Target, arguments = none}}.


ping(Request, State = #state{}) ->
    {pong, Request, State}.


allowed_methods (Request, State = #state{}) ->
	Outcome = {ok, ['GET']},
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


malformed_request (Request, OldState = #state{target = Target, arguments = none}) ->
	Outcome = case Target of
		{nodes} ->
			mosaic_webmachine:enforce_get_request ([], Request);
		{nodes, self, Operation} when ((Operation =:= activate) orelse (Operation =:= deactivate)) ->
			mosaic_webmachine:enforce_get_request ([], Request);
		{ring} ->
			mosaic_webmachine:enforce_get_request ([], Request);
		{ring, Operation} when ((Operation =:= include) orelse (Operation =:= exclude)) ->
			case mosaic_webmachine:enforce_get_request (
					[{<<"node">>, fun mosaic_generic_coders:decode_atom/1}],
					Request) of
				{ok, false, [Node]} ->
					{ok, false, OldState#state{arguments = dict:from_list ([{node, Node}])}};
				Error = {error, true, _Reason} ->
					Error
			end;
		{ring, reboot} ->
			mosaic_webmachine:enforce_get_request ([], Request)
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, OldState).


content_types_provided (Request, State = #state{}) ->
	Outcome = {ok, [{"application/json", handle_as_json}]},
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


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
