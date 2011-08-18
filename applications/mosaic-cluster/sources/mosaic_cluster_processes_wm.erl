
-module (mosaic_cluster_processes_wm).


-export ([init/1, allowed_methods/2, content_types_provided/2, malformed_request/2, handle_as_json/2, ping/2]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


-dispatch ({["processes", "nodes"], {nodes}}).
-dispatch ({["processes", "nodes", "self", "activate"], {nodes, self, activate}}).
-dispatch ({["processes", "nodes", "self", "deactivate"], {nodes, self, deactivate}}).
-dispatch ({["processes"], {processes}}).
-dispatch ({["processes", "create"], {processes, create}}).
-dispatch ({["processes", "stop"], {processes, stop}}).
-dispatch ({["processes", "call"], {processes, call}}).
-dispatch ({["processes", "cast"], {processes, cast}}).
-dispatch ({["processes", "ping"], {ping}}).


-record (state, {target, arguments}).


init (Target) ->
	{ok, #state{target = Target, arguments = none}}.

ping(Request, State = #state{}) ->
    {pong, Request, State}.

allowed_methods (Request, State = #state{}) ->
	Outcome = {ok, ['GET']},
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).

content_types_provided (Request, State = #state{}) ->
	Outcome = {ok, [{"application/json", handle_as_json}]},
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


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
						{"type", fun mosaic_generic_coders:decode_string/1},
						{"configuration", fun mosaic_json_coders:decode_json/1},
						{"count", fun mosaic_generic_coders:decode_integer/1}],
					Request) of
				{ok, false, [Type, Configuration, Count]} ->
					if
						(Count > 0), (Count =< 128) ->
							{ok, false, State#state{arguments = dict:from_list ([{type, Type}, {configuration, Configuration}, {count, Count}])}};
						true ->
							{error, {invalid_argument, "count", {out_of_range, 1, 128}}}
					end;
				Error = {error, _Reason} ->
					Error
			end;
		{processes, stop} ->
			case mosaic_webmachine:enforce_request ('GET', [{"key", fun mosaic_generic_coders:decode_string/1}], Request) of
				{ok, false, [Key]} ->
					{ok, false, State#state{arguments = dict:from_list ([{key, Key}])}};
				Error = {error, _Reason} ->
					Error
			end;
		{processes, Action} when ((Action =:= call) orelse (Action =:= cast)) ->
			case mosaic_webmachine:enforce_request ('GET',
					[
						{"key", fun mosaic_generic_coders:decode_string/1},
						{"operation", fun mosaic_generic_coders:decode_string/1},
						{"inputs", fun mosaic_json_coders:decode_json/1}],
					Request) of
				{ok, false, [Key, Operation, Inputs]} ->
					{ok, false, State#state{arguments = dict:from_list ([{key, Key}, {operation, Operation}, {inputs, Inputs}])}};
				Error = {error, _Reason} ->
					Error
			end;
		{ping} ->
			case mosaic_webmachine:enforce_request ('GET', [{"count", fun mosaic_generic_coders:decode_integer/1}], Request) of
				{ok, false, [Count]} ->
					if
						Count =:= 0 ->
							{ok, false, State#state{arguments = dict:from_list ([{count, default}])}};
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
			case mosaic_cluster_processes:service_nodes () of
				{ok, Nodes} ->
					{ok, json_struct, [
							{self, mosaic_generic_coders:encode_atom (erlang:node ())},
							{nodes, [enforce_ok_1 (mosaic_generic_coders:encode_atom (Node)) || Node <- Nodes]}]};
				Error = {error, _Reason} ->
					Error
			end;
		{nodes, self, activate} ->
			case mosaic_cluster_processes:service_activate () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{nodes, self, deactivate} ->
			case mosaic_cluster_processes:service_deactivate () of
				ok ->
					ok;
				Error = {error, _Reason} ->
					Error
			end;
		{processes} ->
			case mosaic_cluster_processes:list () of
				{ok, Keys, []} ->
					{ok, json_struct, [
							{keys, [enforce_ok_1 (mosaic_component_coders:encode_component (Key)) || Key <- Keys]}]};
				{ok, Keys, Reasons} ->
					{ok, json_struct, [
							{keys, [enforce_ok_1 (mosaic_component_coders:encode_component (Key)) || Key <- Keys]},
							{error, [enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason)) || Reason <- Reasons]}]};
				Error = {error, _Reason} ->
					Error
			end;
		{processes, create} ->
			try
				ProcessType = case dict:fetch (type, Arguments) of
					<<$#, ProcessType_ / binary>> ->
						case mosaic_generic_coders:decode_atom (ProcessType_) of
							{ok, ProcessType__} ->
								ProcessType__;
							Error1 = {error, _Reason1} ->
								throw (Error1)
						end;
					ProcessType_ ->
						throw ({error, {invalid_type, ProcessType_}})
				end,
				ProcessConfigurationContent = dict:fetch (configuration, Arguments),
				Count = dict:fetch (count, Arguments),
				case mosaic_cluster_processes:define_and_create (ProcessType, json, ProcessConfigurationContent, Count) of
					{ok, Processes, []} ->
						{ok, json_struct, [
								{keys, [enforce_ok_1 (mosaic_component_coders:encode_component (Key)) || {Key, _Process} <- Processes]}]};
					{ok, Processes, Reasons} ->
						{ok, json_struct, [
								{keys, [enforce_ok_1 (mosaic_component_coders:encode_component (Key)) || {Key, _Process} <- Processes]},
								{error, [enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason)) || Reason <- Reasons]}]};
					Error2 = {error, _Reason2} ->
						Error2
				end
			catch throw : Error = {error, _Reason} -> Error end;
		{processes, stop} ->
			try
				Key = case dict:fetch (key, Arguments) of
					<<$#, Alias_ / binary>> ->
						case mosaic_cluster_processes_router:resolve_alias (Alias_) of
							{ok, Key_} ->
								Key_;
							{error, _Reason1} ->
								throw ({error, {unresolved_alias, Alias_}})
						end;
					Key_ when (byte_size (Key_) =:= 40) ->
						case mosaic_component_coders:decode_component (Key_) of
							{ok, Key__} ->
								Key__;
							{error, _Reason1} ->
								throw ({error, {invalid_component, Key_, invalid_content}})
						end;
					Key_ ->
						throw ({error, {invalid_component, Key_, invalid_length}})
				end,
				case mosaic_process_router:resolve (Key) of
					{ok, Process} ->
						case mosaic_process:stop (Process) of
							ok ->
								ok;
							Error2 = {error, _Reason2} ->
								Error2
						end;
					Error2 = {error, _Reason2} ->
						Error2
				end
			catch throw : Error = {error, _Reason} -> Error end;
		{processes, Action} when ((Action =:= call) orelse (Action =:= cast)) ->
			try
				Key = case dict:fetch (key, Arguments) of
					<<$#, Alias_ / binary>> ->
						case mosaic_cluster_processes_router:resolve_alias (Alias_) of
							{ok, Key_} ->
								Key_;
							{error, _Reason1} ->
								throw ({error, {unresolved_alias, Alias_}})
						end;
					Key_ when (byte_size (Key_) =:= 40) ->
						case mosaic_component_coders:decode_component (Key_) of
							{ok, Key__} ->
								Key__;
							{error, _Reason1} ->
								throw ({error, {invalid_component, Key_, invalid_content}})
						end;
					Key_ ->
						throw ({error, {invalid_component, Key_, invalid_length}})
				end,
				Operation = dict:fetch (operation, Arguments),
				Inputs = dict:fetch (inputs, Arguments),
				case Action of
					call ->
						case mosaic_process_router:call (Key, Operation, Inputs, <<>>, undefined) of
							{ok, Outputs, _Data} ->
								case mosaic_json_coders:coerce_json (Outputs) of
									{ok, CoercedOutputs} ->
										{ok, json_struct, [{ok, true}, {outputs, CoercedOutputs}]};
									{error, _Reason2} ->
										{ok, EncodedOutputs} = mosaic_generic_coders:encode_term (json, Outputs),
										{ok, json_struct, [{ok, true}, {outputs, EncodedOutputs}]}
								end;
							{error, Reason2, _Data} ->
								case mosaic_json_coders:coerce_json (Reason2) of
									{ok, CoercedReason2} ->
										{ok, json_struct, [{ok, false}, {error, CoercedReason2}]};
									{error, _Reason2} ->
										{ok, EncodedReason2} = mosaic_generic_coders:encode_term (json, Reason2),
										{ok, json_struct, [{ok, true}, {error, EncodedReason2}]}
								end;
							Error2 = {error, _Reason2} ->
								Error2;
							CallReply ->
								{error, {invalid_reply, CallReply}}
						end;
					cast ->
						case mosaic_process_router:cast (Key, Operation, Inputs, <<>>) of
							ok ->
								ok;
							Error2 = {error, _Reason2} ->
								Error2;
							CastReply ->
								{error, {invalid_reply, CastReply}}
						end
				end
			catch throw : Error = {error, _Reason} -> Error end;
		{ping} ->
			Count = dict:fetch (count, Arguments),
			case mosaic_cluster_processes:service_ping (Count) of
				{ok, Pongs, Pangs} ->
					{ok, json_struct, [
							{pongs, [
								{struct, [
										{key, enforce_ok_1 (mosaic_component_coders:encode_component (Key))},
										{partition, enforce_ok_1 (mosaic_component_coders:encode_component (<<Partition : 160>>))},
										{node, enforce_ok_1 (mosaic_generic_coders:encode_atom (Node))}]}
								|| {pong, Key, _Vnode, mosaic_cluster_processes, {Partition, Node}} <- Pongs]},
							{pangs, [
								{struct, [
										{key, enforce_ok_1 (mosaic_component_coders:encode_component (Key))},
										{partition, enforce_ok_1 (mosaic_component_coders:encode_component (<<Partition : 160>>))},
										{node, enforce_ok_1 (mosaic_generic_coders:encode_atom (Node))},
										{reason, enforce_ok_1 (mosaic_generic_coders:encode_reason (json, Reason))}]}
								|| {pang, Key, {Partition, Node}, Reason} <- Pangs]}]};
				Error = {error, _Reason} ->
					Error
			end
	end,
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).
