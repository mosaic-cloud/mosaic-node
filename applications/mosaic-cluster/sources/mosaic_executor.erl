
-module (mosaic_executor).

-export ([nodes/0]).
-export ([service_activate/0, service_deactivate/0]).
-export ([define_and_create_process/2, define_and_create_processes/3]).
-export ([define_process/2, define_processes/3, define_processes/4]).
-export ([create_process/1, create_processes/1, create_processes/2]).
-export ([stop_process/1, stop_process/2, stop_processes/2, stop_processes/3]).
-export ([ping/0, ping/1, ping/2]).


nodes () ->
	Nodes = riak_core_node_watcher:nodes (mosaic_executor),
	{ok, Nodes}.


service_activate () ->
	mosaic_executor_vnode:service_activate ().

service_deactivate () ->
	mosaic_executor_vnode:service_deactivate ().


define_and_create_process (Module, Arguments)
		when is_atom (Module) ->
	case define_process (Module, Arguments) of
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


define_and_create_processes (Module, Arguments, Count)
		when is_atom (Module), is_integer (Count), (Count > 0) ->
	{ok, Keys, DefineReasons} = define_processes (Module, Arguments, Count),
	{ok, Processes, CreateReasons} = create_processes (Keys),
	{ok, Processes, DefineReasons ++ CreateReasons}.


define_process (Module, Arguments)
		when is_atom (Module) ->
	case define_processes (Module, Arguments, 1) of
		{ok, [], []} ->
			{error, vnode_unreachable};
		{ok, [Key], []} ->
			{ok, Key};
		{ok, [], [{_Key, Reason}]} ->
			{error, Reason}
	end.

define_processes (Module, Arguments, Count)
		when is_atom (Module), is_integer (Count), Count > 0 ->
	define_processes (Module, Arguments, Count, Count * 2).

define_processes (Module, Arguments, Count, Retries)
		when is_atom (Module), is_integer (Count), is_integer (Retries), Count > 0, Retries >= Count ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	Fuzz = erlang:make_ref (),
	{ok, _, {Keys, Reasons}, _} = sync_command (
			fun (Index) ->
				Key = chash:key_of (term_to_binary ({Fuzz, Index})),
				{ok, Targets} = mosaic_cluster:targets (Key, mosaic_executor, 1, active_without_fallbacks),
				{ok, Key, {define_process, Key, Module, Arguments}, Service, Master, Targets, Index + 1}
			end, 0,
			fun ({Key, {define_process, Key, _, _}, _, _, _}, [{_, Reply}], {Keys, Reasons}) ->
				case Reply of
					{ok, Key} ->
						{ok, {[Key | Keys], Reasons}};
					{error, Reason} ->
						{ok, {Keys, [{Key, Reason} | Reasons]}};
					_ ->
						{ok, {Keys, [{Key, {invalid_reply, Reply}} | Reasons]}}
				end
			end, {[], []},
			Count, Retries),
	{ok, lists:reverse (Keys), lists:reverse (Reasons)}.


create_process (Key) ->
	case create_processes ([Key]) of
		{ok, [], []} ->
			{error, vnode_unreachable};
		{ok, [{Key, Process}], []} ->
			{ok, Process};
		{ok, [], [{Key, Reason}]} ->
			{error, Reason}
	end.

create_processes (Keys)
		when is_list (Keys), (Keys =/= []) ->
	create_processes (Keys, erlang:length (Keys) * 2).

create_processes (Keys, Retries)
		when is_list (Keys), is_integer (Retries), Retries > 0, Retries >= length (Keys) ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	{ok, _, {SuccessfullKeys, Reasons}, _} = sync_command (
			fun ([Key | RemainingKeys]) ->
				{ok, Targets} = mosaic_cluster:targets (Key, mosaic_executor, 1, active_without_fallbacks),
				{ok, Key, {create_process, Key}, Service, Master, Targets, RemainingKeys}
			end, Keys,
			fun ({Key, {create_process, Key}, _, _, _}, [{_, Reply}], {SuccessfullKeys, Reasons}) ->
				case Reply of
					{ok, Process} ->
						{ok, {[{Key, Process} | SuccessfullKeys], Reasons}};
					{error, Reason} ->
						{ok, {SuccessfullKeys, [{Key, Reason} | Reasons]}};
					_ ->
						{ok, {SuccessfullKeys, [{Key, {invalid_reply, Reply}} | Reasons]}}
				end
			end, {[], []},
			erlang:length (Keys), Retries),
	{ok, lists:reverse (SuccessfullKeys), lists:reverse (Reasons)}.


stop_process (Key) ->
	stop_process (Key, normal).

stop_process (Key, Signal) ->
	case stop_processes ([Key], Signal) of
		{ok, [], []} ->
			{error, vnode_unreachable};
		{ok, [Key], []} ->
			ok;
		{ok, [], [{Key, Reason}]} ->
			{error, Reason}
	end.

stop_processes (Keys, Signal)
		when is_list (Keys), (Keys =/= []) ->
	stop_processes (Keys, Signal, erlang:length (Keys) * 2).

stop_processes (Keys, Signal, Retries)
		when is_list (Keys), is_integer (Retries), Retries > 0, Retries >= length (Keys) ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	{ok, _, {SuccessfullKeys, Reasons}, _} = sync_command (
			fun ([Key | RemainingKeys]) ->
				{ok, Targets} = mosaic_cluster:targets (Key, mosaic_executor, 1, active_without_fallbacks),
				{ok, Key, {stop_process, Key, Signal}, Service, Master, Targets, RemainingKeys}
			end, Keys,
			fun ({Key, {stop_process, Key, _}, _, _, _}, [{_, Reply}], {SuccessfullKeys, Reasons}) ->
				case Reply of
					ok ->
						{ok, {[Key | SuccessfullKeys], Reasons}};
					{error, Reason} ->
						{ok, {SuccessfullKeys, [{Key, Reason} | Reasons]}};
					_ ->
						{ok, {SuccessfullKeys, [{Key, {invalid_reply, Reply}} | Reasons]}}
				end
			end, {[], []},
			erlang:length (Keys), Retries),
	{ok, lists:reverse (SuccessfullKeys), lists:reverse (Reasons)}.


ping () ->
	ping (4).

ping (Count)
		when is_integer (Count), (Count > 0) ->
	ping (Count, Count * 2).

ping (Count, Retries)
		when is_integer (Count), is_integer (Retries), (Count > 0), (Retries >= Count) ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	Fuzz = erlang:make_ref (),
	{ok, _, {PingCount, Outcomes}, _} = sync_command (
			fun (Index) ->
				Key = chash:key_of (term_to_binary ({Fuzz, Index})),
				{ok, Targets} = mosaic_cluster:targets (Key, mosaic_executor, 1, primaries),
				{ok, Key, {ping, Key}, Service, Master, Targets, Index + 1}
			end, 0,
			fun ({Key, {ping, Key}, _, _, _}, [{Target, Reply}], {PingCount, Outcomes}) ->
				Outcome = case Reply of
					{pong, Key, Target} ->
						{pong, Target};
					{pong, OtherKey, Target} ->
						{pang, Target, {mismatched_key, OtherKey, Target}};
					{pong, _, OtherTarget} ->
						{pang, Target, {mismatched_target, OtherTarget, Target}};
					{error, Reason} ->
						{pang, Target, Reason};
					_ ->
						{pang, Target, {invalid_reply, Reply}}
				end,
				case Outcome of
					{pong, _} ->
						{ok, {PingCount + 1, [Outcome | Outcomes]}};
					_ ->
						{ok, {PingCount, [Outcome | Outcomes]}}
				end
			end, {0, []},
			Count, Retries),
	{ok, PingCount, lists:reverse (Outcomes)}.


sync_command (RequestFun, RequestState, RepliesFun, RepliesState, Count, Retries)
		when is_function (RequestFun, 1), is_function (RepliesFun, 3),
				is_integer (Count), is_integer (Retries), Count >= 0, Retries >= Count ->
	sync_command1 (RequestFun, RequestState, RepliesFun, RepliesState, undefined, Count, Retries).

sync_command1 (_RequestFun, RequestState, _RepliesFun, RepliesState, PendingRequest, 0, _Retries) ->
	{ok, RequestState, RepliesState, PendingRequest};
	
sync_command1 (_RequestFun, RequestState, _RepliesFun, RepliesState, PendingRequest, _Count, 0) ->
	{ok, RequestState, RepliesState, PendingRequest};
	
sync_command1 (RequestFun, OldRequestState, RepliesFun, RepliesState, undefined, Count, Retries) ->
	{ok, Key, Command, Service, Master, Targets, NewRequestState} = RequestFun (OldRequestState),
	sync_command1 (RequestFun, NewRequestState, RepliesFun, RepliesState, {Key, Command, Service, Master, Targets}, Count, Retries);
	
sync_command1 (RequestFun, RequestState, RepliesFun, OldRepliesState, PendingRequest = {Key, Command, Service, Master, Targets}, Count, Retries) ->
	case Targets of
		Targets when is_list (Targets) ->
			Replies = lists:map (
					fun (Target) ->
						Outcome = try
							riak_core_vnode_master:sync_command (Target, Command, Master)
						catch
							exit : {Reason, _Call} ->
								{error, Reason}
						end,
						{Target, Outcome}
					end, Targets),
			{ok, NewRepliesState} = RepliesFun (PendingRequest, Replies, OldRepliesState),
			sync_command1 (RequestFun, RequestState, RepliesFun, NewRepliesState, undefined, Count - 1, Retries);
		[] ->
			sync_command1 (RequestFun, RequestState, RepliesFun, OldRepliesState, PendingRequest, Count, Retries - 1)
	end.
