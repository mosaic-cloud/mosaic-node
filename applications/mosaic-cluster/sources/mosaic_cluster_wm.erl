
-module (mosaic_cluster_wm).

-export ([init/1, allowed_methods/2, content_types_provided/2, malformed_request/2, handle_as_json/2, ping/2]).


-dispatch ({["cluster", "nodes"], {nodes}}).
-dispatch ({["cluster", "ring"], {ring}}).
-dispatch ({["cluster", "ring", "include", node], {ring, include}}).
-dispatch ({["cluster", "ring", "exclude", node], {ring, exclude}}).


-record (state, {target}).


init (Target) ->
	{ok, #state{target = Target}}.

ping(Request, State = #state{}) ->
    {pong, Request, State}.

allowed_methods (Request, State = #state{}) ->
	{['GET'], Request, State}.

content_types_provided (Request, State = #state{}) ->
	{[{"application/json", handle_as_json}], Request, State}.


malformed_request (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{nodes} ->
			ok;
		{ring} ->
			ok;
		{ring, Operation} when ((Operation =:= include) orelse (Operation =:= exclude)) ->
			case wrq:path_info (node, Request) of
				Node when is_list (Node) ->
					try
						_ = erlang:list_to_existing_atom (Node),
						ok
					catch
						error : badarg ->
							{error, {invalid_node, Node}}
					end
			end
	end,
	case Outcome of
		ok ->
			{false, Request, State};
		{error, Reason} ->
			mosaic_webmachine:return_with_content (true, error, Reason, Request, State)
	end.


handle_as_json (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{nodes} ->
			{ok, json, {struct, [{ok, true}, {self, erlang:node ()}, {peers, erlang:nodes ()}]}};
		{ring} ->
			{ok, Ring} = riak_core_ring_manager:get_my_ring (),
			RingNodes = riak_core_ring:all_members (Ring),
			RingPartitions = riak_core_ring:all_owners (Ring),
			{ok, json, {struct, [
					{ok, true},
					{nodes, RingNodes},
					{partitions, lists:map (
							fun ({Key, Node}) ->
								{struct, [{key, erlang:list_to_binary (erlang:integer_to_list (Key))}, {node, Node}]}
							end, RingPartitions)}]}};
		{ring, include} ->
			Node = erlang:list_to_existing_atom (wrq:path_info (node, Request)),
			case mosaic_cluster:ring_include (Node) of
				ok ->
					{ok, json, {struct, [{ok, true}]}};
				Error = {error, _Reason} ->
					Error
			end;
		{ring, exclude} ->
			Node = erlang:list_to_existing_atom (wrq:path_info (node, Request)),
			case mosaic_cluster:ring_exclude (Node) of
				ok ->
					{ok, json, {struct, [{ok, true}]}};
				Error = {error, _Reason} ->
					Error
			end
	end,
	case Outcome of
		{ok, json, ContentTerm} ->
			mosaic_webmachine:respond_with_content (json, ContentTerm, Request, State);
		{error, Reason} ->
			mosaic_webmachine:respond_with_content (error, Reason, Request, State)
	end.
