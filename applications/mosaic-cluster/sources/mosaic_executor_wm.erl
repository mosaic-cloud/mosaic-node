
-module (mosaic_executor_wm).

-export ([init/1, allowed_methods/2, content_types_provided/2, malformed_request/2, handle_as_json/2, ping/2]).


-dispatch ({["services", "executor", "nodes"], {nodes}}).
-dispatch ({["services", "executor", "nodes", "self", "activate"], {nodes, self, activate}}).
-dispatch ({["services", "executor", "nodes", "self", "deactivate"], {nodes, self, deactivate}}).
-dispatch ({["services", "executor", "processes", "create"], {processes, create}}).
-dispatch ({["services", "executor", "processes", "stop"], {processes, stop}}).
-dispatch ({["services", "executor", "ping"], {ping}}).


-record (state, {target, arguments}).


init (Target) ->
	{ok, #state{target = Target, arguments = none}}.

ping(Request, State = #state{}) ->
    {pong, Request, State}.

allowed_methods (Request, State = #state{}) ->
	{['GET'], Request, State}.

content_types_provided (Request, State = #state{}) ->
	{[{"application/json", handle_as_json}], Request, State}.


malformed_request (Request, State = #state{target = Target, arguments = none}) ->
	Outcome = case Target of
		{nodes} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{nodes, self, Operation} when ((Operation =:= activate) orelse (Operation =:= deactivate)) ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{ping} ->
			case mosaic_webmachine:enforce_request ('GET', ["count"], Request) of
				{ok, false, [CountString]} ->
					try
						Count = erlang:list_to_integer (CountString),
						if
							(Count > 0), (Count =< 128) ->
								{ok, false, State#state{arguments = dict:from_list ([{count, Count}])}};
							true ->
								{error, {invalid_query, {invalid_count, CountString}}}
						end
					catch
						error : badarg ->
							{error, {invalid_query, {invalid_count, CountString}}}
					end;
				Error = {error, _Reason} ->
					Error
			end
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


handle_as_json (Request, State = #state{target = Target, arguments = Arguments}) ->
	Outcome = case Target of
		{nodes} ->
			case mosaic_executor:nodes () of
				{ok, Nodes} ->
					{ok, json_struct, [{nodes, Nodes}]};
				Error = {error, _Reason} ->
					Error
			end;
		{nodes, self, activate} ->
			case mosaic_executor:service_activate () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{nodes, self, deactivate} ->
			case mosaic_executor:service_deactivate () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{create} ->
			ok;
		{ping} ->
			Count = dict:fetch (count, Arguments),
			case mosaic_executor:ping (Count) of
				{ok, _Count, Pings} ->
					Pongs = lists:filter (fun ({pong, _}) -> true; ({pang, _, _}) -> false end, Pings),
					Pangs = lists:filter (fun ({pang, _, _}) -> true; ({pong, _}) -> false end, Pings),
					{ok, json_struct, [
							{pongs, lists:map (
									fun ({pong, {Key, Node}}) ->
										{struct, [{key, erlang:list_to_binary (erlang:integer_to_list (Key))}, {node, Node}]}
									end, Pongs)},
							{pangs, lists:map (
									fun ({pang, {Key, Node}, Reason}) ->
										{struct, [
												{key, erlang:list_to_binary (erlang:integer_to_list (Key))}, {node, Node},
												{reason, erlang:iolist_to_binary (io_lib:format ("~76p", [Reason]))}]}
									end, Pangs)}]};
				Error = {error, _Reason} ->
					Error
			end
	end,
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).
