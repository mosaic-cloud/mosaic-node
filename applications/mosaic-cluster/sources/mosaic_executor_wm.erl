
-module (mosaic_executor_wm).

-export ([init/1, allowed_methods/2, content_types_provided/2, malformed_request/2, handle_as_json/2, ping/2]).


-dispatch ({["executor", "nodes"], {nodes}}).
-dispatch ({["executor", "nodes", "self", "activate"], {nodes, self, activate}}).
-dispatch ({["executor", "nodes", "self", "deactivate"], {nodes, self, deactivate}}).
-dispatch ({["executor", "processes"], {processes}}).
-dispatch ({["executor", "processes", "create"], {processes, create}}).
-dispatch ({["executor", "processes", "stop"], {processes, stop}}).
-dispatch ({["executor", "ping"], {ping}}).


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
		{processes} ->
			mosaic_webmachine:enforce_request ('GET', [], Request);
		{processes, create} ->
			case mosaic_webmachine:enforce_request ('GET',
					[
						{"type", fun mosaic_webmachine:parse_existing_atom/1},
						{"arguments", fun mosaic_webmachine:parse_json/1},
						{"count", fun mosaic_webmachine:parse_integer/1}],
					Request) of
				{ok, false, [Type, Arguments, Count]} ->
					if
						(Count > 0), (Count =< 128) ->
							{ok, false, State#state{arguments = dict:from_list ([{type, Type}, {arguments, Arguments}, {count, Count}])}};
						true ->
							{error, {invalid_argument, "count", {out_of_range, 1, 128}}}
					end;
				Error = {error, _Reason} ->
					Error
			end;
		{processes, stop} ->
			case mosaic_webmachine:enforce_request ('GET', [{"key", fun mosaic_webmachine:parse_hex_binary_key/1}], Request) of
				{ok, false, [Key]} ->
					{ok, false, State#state{arguments = dict:from_list ([{key, Key}])}};
				Error = {error, _Reason} ->
					Error
			end;
		{ping} ->
			case mosaic_webmachine:enforce_request ('GET', [{"count", fun mosaic_webmachine:parse_integer/1}], Request) of
				{ok, false, [Count]} ->
					if
						(Count > 0), (Count =< 128) ->
							{ok, false, State#state{arguments = dict:from_list ([{count, Count}])}};
						true ->
							{error, {invalid_argument, "count", {out_of_range, 1, 128}}}
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
					{ok, json_struct, [
							{nodes, lists:map (fun mosaic_webmachine:format_atom/1, Nodes)}]};
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
		{processes} ->
			case mosaic_executor:select_processes () of
				{ok, Keys, []} ->
					{ok, json_struct, [
							{keys, lists:map (fun mosaic_webmachine:format_binary_key/1, Keys)}]};
				{ok, Keys, Reasons} ->
					{ok, json_struct, [
							{keys, lists:map (fun mosaic_webmachine:format_binary_key/1, Keys)},
							{error, lists:map (fun mosaic_webmachine:format_term/1, Reasons)}]};
				Error = {error, _Reason} ->
					Error
			end;
		{processes, create} ->
			ProcessType = dict:fetch (type, Arguments),
			ProcessArgumentsContent = dict:fetch (arguments, Arguments),
			Count = dict:fetch (count, Arguments),
			case mosaic_executor:define_and_create_processes (ProcessType, json, ProcessArgumentsContent, Count) of
				{ok, Keys, _Processes, []} ->
					{ok, json_struct, [
							{keys, lists:map (fun mosaic_webmachine:format_binary_key/1, Keys)}]};
				{ok, Keys, _Processes, Reasons} ->
					{ok, json_struct, [
							{keys, lists:map (fun mosaic_webmachine:format_binary_key/1, Keys)},
							{errors, lists:map (fun mosaic_webmachine:format_term/1, Reasons)}]};
				Error = {error, _Reason} ->
					Error
			end;
		{processes, stop} ->
			Key = dict:fetch (key, Arguments),
			case mosaic_executor:stop_process (Key) of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{ping} ->
			Count = dict:fetch (count, Arguments),
			case mosaic_executor:ping (Count) of
				{ok, Pongs, Pangs} ->
					{ok, json_struct, [
							{pongs, lists:map (
									fun ({pong, Key, {Partition, Node}}) ->
										{struct, [
												{key, mosaic_webmachine:format_binary_key (Key)},
												{partition, mosaic_webmachine:format_numeric_key (Partition)},
												{node, mosaic_webmachine:format_atom (Node)}]}
									end, Pongs)},
							{pangs, lists:map (
									fun ({pang, Key, {Partition, Node}, Reason}) ->
										{struct, [
												{key, mosaic_webmachine:format_binary_key (Key)},
												{partition, mosaic_webmachine:format_numeric_key (Partition)},
												{node, mosaic_webmachine:format_atom (Node)},
												{reason, mosaic_webmachine:format_term (Reason)}]}
									end, Pangs)}]};
				Error = {error, _Reason} ->
					Error
			end
	end,
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).
