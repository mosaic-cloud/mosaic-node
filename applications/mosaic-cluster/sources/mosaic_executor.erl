
-module (mosaic_executor).

-export ([nodes/0]).
-export ([service_activate/0, service_deactivate/0]).
-export ([define_and_create_process/3, define_and_create_processes/4]).
-export ([define_process/3, define_processes/4]).
-export ([create_process/1, create_processes/1]).
-export ([resolve_process/1, resolve_processes/1]).
-export ([stop_process/1, stop_process/2, stop_processes/1, stop_processes/2]).
-export ([select_processes/0]).
-export ([ping/0, ping/1]).


nodes () ->
	Nodes = riak_core_node_watcher:nodes (mosaic_executor),
	{ok, Nodes}.


service_activate () ->
	mosaic_executor_vnode:service_activate ().

service_deactivate () ->
	mosaic_executor_vnode:service_deactivate ().


define_and_create_process (Type, ArgumentsEncoding, ArgumentsContent)
		when is_atom (Type), is_atom (ArgumentsEncoding) ->
	case define_process (Type, ArgumentsEncoding, ArgumentsContent) of
		{ok, Key} ->
			case create_process (Key) of
				{ok, Process} ->
					{ok, Key, Process};
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.

define_and_create_processes (Type, ArgumentsEncoding, ArgumentsContent, Count)
		when is_atom (Type), is_atom (ArgumentsEncoding), is_integer (Count), (Count > 0) ->
	case define_processes (Type, ArgumentsEncoding, ArgumentsContent, Count) of
		{ok, [], DefineReasons} ->
			{ok, [], [], DefineReasons};
		{ok, DefineKeys, DefineReasons} when is_list (DefineKeys) ->
			case create_processes (DefineKeys) of
				{ok, CreateProcesses, CreateReasons} ->
					{ok, DefineKeys, CreateProcesses, DefineReasons ++ CreateReasons}
			end
	end.


define_process (Type, ArgumentsEncoding, ArgumentsContent)
		when is_atom (Type), is_atom (ArgumentsEncoding) ->
	case define_processes (Type, ArgumentsEncoding, ArgumentsContent, 1) of
		{ok, [], []} ->
			{error, unreachable_vnode};
		{ok, [Key], []} ->
			{ok, Key};
		{ok, [], [{_Target, _Key, Reason}]} ->
			{error, Reason}
	end.

define_processes (Type, ArgumentsEncoding, ArgumentsContent, Count)
		when is_atom (Type), is_atom (ArgumentsEncoding), is_integer (Count), (Count > 0) ->
	{ok, Keys} = mosaic_cluster:keys (Count),
	request_reply_sync_command (
			fun (Key) -> {define_process, Key, Type, ArgumentsEncoding, ArgumentsContent} end,
			fun (Key, _, Reply) ->
				case Reply of
					{ok, Key} ->
						{outcome, Key};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys,
			mosaic_executor,
			riak_core_vnode_master:reg_name (mosaic_executor_vnode),
			1).


create_process (Key) ->
	case create_processes ([Key]) of
		{ok, [], []} ->
			{error, unreachable_vnode};
		{ok, [{Key, Process}], []} ->
			{ok, Process};
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason}
	end.

create_processes (Keys)
		when is_list (Keys), (Keys =/= []) ->
	request_reply_sync_command (
			fun (Key) -> {create_process, Key} end,
			fun (Key, _, Reply) ->
				case Reply of
					{ok, Process} ->
						{outcome, {Key, Process}};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys,
			mosaic_executor,
			riak_core_vnode_master:reg_name (mosaic_executor_vnode),
			1).


resolve_process (Key) ->
	case resolve_processes ([Key]) of
		{ok, [], []} ->
			{error, unreachable_vnode};
		{ok, [{Key, Process}], []} ->
			{ok, Process};
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason}
	end.

resolve_processes (Keys)
		when is_list (Keys), (Keys =/= []) ->
	request_reply_sync_command (
			fun (Key) -> {resolve_process, Key} end,
			fun (Key, _, Reply) ->
				case Reply of
					{ok, Process} ->
						{outcome, {Key, Process}};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys,
			mosaic_executor,
			riak_core_vnode_master:reg_name (mosaic_executor_vnode),
			1).


stop_process (Key) ->
	stop_process (Key, normal).

stop_process (Key, Signal) ->
	case stop_processes ([Key], Signal) of
		{ok, [], []} ->
			{error, unreachable_vnode};
		{ok, [Key], []} ->
			ok;
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason}
	end.

stop_processes (Keys) ->
	stop_processes (Keys, normal).

stop_processes (Keys, Signal)
		when is_list (Keys), (Keys =/= []) ->
	request_reply_sync_command (
			fun (Key) -> {stop_process, Key, Signal} end,
			fun (Key, _, Reply) ->
				case Reply of
					ok ->
						{outcome, Key};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys,
			mosaic_executor,
			riak_core_vnode_master:reg_name (mosaic_executor_vnode),
			1).


select_processes () ->
	{ok, Targets} = mosaic_cluster:targets (mosaic_executor, primaries),
	TargetKeys = lists:map (fun ({Partition, _Node}) -> <<(Partition - 1):160>> end, Targets),
	{ok, ProcessKeys, Reasons} = request_reply_sync_command (
			fun (_TargetKey) -> {select_processes} end,
			fun (_TargetKey, _, Reply) ->
				case Reply of
					{ok, Keys} ->
						{outcome, Keys};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			TargetKeys,
			mosaic_executor,
			riak_core_vnode_master:reg_name (mosaic_executor_vnode),
			1),
	{ok, lists:usort (lists:flatten (ProcessKeys)), Reasons}.


ping () ->
	ping (4).

ping (Count)
		when is_integer (Count), (Count > 0) ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	{ok, Keys} = mosaic_cluster:keys (Count),
	{ok, [], {Pongs, Pangs}} = sync_command (
			fun
				([]) ->
					{finish, []};
				([Key | PendingKeys]) ->
					{ok, Targets} = mosaic_cluster:targets (Key, mosaic_executor, 1, primaries),
					Command = {ping, Key},
					Request = {Key, Command, Service, Master, Targets},
					{continue, Request, PendingKeys}
			end, Keys,
			fun ({Key, {ping, Key}, _, _, _}, Replies, {CollectedPongs, CollectedPangs}) ->
				Outcomes = lists:map (
						fun ({Target, Reply}) ->
							case Reply of
								Pong = {pong, Key, Target} ->
									Pong;
								{pong, OtherKey, Target} ->
									{pang, Key, Target, {mismatched_key, OtherKey}};
								{pong, _, OtherTarget} ->
									{pang, Key, Target, {mismatched_target, OtherTarget}};
								{error, Reason} ->
									{pang, Key, Target, Reason};
								_ ->
									{pang, Key, Target, {invalid_reply, Reply}}
							end
						end,
						Replies),
				Pongs = lists:filter (fun ({pong, _, _}) -> true; ({pang, _, _, _}) -> false end, Outcomes),
				Pangs = lists:filter (fun ({pang, _, _, _}) -> true; ({pong, _, _}) -> false end, Outcomes),
				{continue, {Pongs ++ CollectedPongs, Pangs ++ CollectedPangs}}
			end, {[], []}),
	{ok, lists:reverse (Pongs), lists:reverse (Pangs)}.


request_reply_sync_command (CommandFunction, OutcomeFunction, Keys, Service, Master, Fanout) ->
	{ok, [], {Outcomes, Reasons}} = sync_command (
			requests_generator (CommandFunction, Service, Master, Fanout), Keys,
			replies_collector (OutcomeFunction), {[], []}),
	{ok, lists:reverse (Outcomes), lists:reverse (Reasons)}.

requests_generator (CommandFunction, Service, Master, Fanout)
		when is_function (CommandFunction, 1), is_atom (Service), is_atom (Master), is_integer (Fanout), (Fanout > 0) ->
	fun
		([]) ->
			{finish, []};
		([Key | PendingKeys]) ->
			{ok, Targets} = mosaic_cluster:targets (Key, mosaic_executor, Fanout, active_without_fallbacks),
			Command = CommandFunction (Key),
			Request = {Key, Command, Service, Master, Targets},
			{continue, Request, PendingKeys}
	end.

replies_collector (OutcomeFunction)
		when is_function (OutcomeFunction, 3) ->
	fun ({Key, Command, _Service, _Master, _Targets}, TargetReplies, {CollectedOutcomes, CollectedReasons}) ->
		Values = lists:map (
				fun ({Target, Reply}) ->
					case OutcomeFunction (Key, Command, Reply) of
						Value = {outcome, _Outcome} ->
							Value;
						{error, Reason} ->
							{error, Target, Key, Reason}
					end
				end, TargetReplies),
		Outcomes = lists:map (
				fun ({outcome, Outcome}) -> Outcome end,
				lists:filter (fun ({outcome, _}) -> true; ({error, _, _, _}) -> false end, Values)),
		Reasons = lists:map (
				fun ({error, Target, Key_, Reason}) when Key_ =:= Key -> {Target, Key_, Reason} end,
				lists:filter (fun ({error, _, _, _}) -> true; ({outcome, _}) -> false end, Values)),
		{continue, {Outcomes ++ CollectedOutcomes, Reasons ++ CollectedReasons}}
	end.


sync_command (RequestFunction, RequestInputState, RepliesFunction, RepliesInputState)
		when is_function (RequestFunction, 1), is_function (RepliesFunction, 3) ->
	case RequestFunction (RequestInputState) of
		{continue, Request = {Key, Command, Service, Master, Targets}, RequestOutputState}
				when is_binary (Key), (bit_size (Key) =:= 160), is_atom (Service), is_atom (Master), is_list (Targets) ->
			TargetReplies = lists:map (
					fun (Target = {Partition, Node})
							when is_atom (Node), is_integer (Partition),
									(Partition >= 0), (Partition < 1461501637330902918203684832716283019655932542975) ->
						Reply = try
							riak_core_vnode_master:sync_command (Target, Command, Master)
						catch
							exit : {Reason, _Call} ->
								{error, Reason}
						end,
						{Target, Reply}
					end, Targets),
			case RepliesFunction (Request, TargetReplies, RepliesInputState) of
				{continue, RepliesOutputState} ->
					sync_command (RequestFunction, RequestOutputState, RepliesFunction, RepliesOutputState);
				{finish, RepliesOutputState} ->
					{ok, RequestOutputState, RepliesOutputState}
			end;
		{finish, RequestOutputState} ->
			{ok, RequestOutputState, RepliesInputState}
	end.
