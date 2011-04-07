
-module (mosaic_executor).

-export ([ping/0, ping/1, ping/2]).
-export ([open_port/2, open_port/3, open_port/4]).

ping () ->
	ping (4).

ping (Count) when is_integer (Count), Count > 0 ->
	ping (Count, Count * 2).

ping (Count, Retries) when is_integer (Count), is_integer (Retries), Count > 0, Retries >= Count ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	Fuzz = erlang:make_ref (),
	{ok, Index, Outcomes, Pending} = sync_command (
			fun (Index) ->
				Key = chash:key_of (term_to_binary ({Fuzz, Index})),
				{ok, Key, {ping, Key}, 1, Service, Master, Index + 1}
			end, 0,
			fun ({Key, {ping, Key}, 1, Service1, Master1}, [{Target, Reply}], Outcomes) when Service1 =:= Service, Master1 =:= Master ->
				Outcome = case Reply of
					{pong, Key, Target} ->
						{pong, Target};
					{pong, OtherKey, Target} ->
						{pang, Target, {missmatched_key, OtherKey, Target}};
					{pong, _, OtherTarget} ->
						{pang, Target, {missmatched_target, OtherTarget, Target}};
					{error, Reason} ->
						{pang, Target, Reason};
					_ ->
						{pang, Target, {invalid_reply, Reply}}
				end,
				{ok, [Outcome | Outcomes]}
			end, [],
			Count, Retries),
	case Pending of
		undefined ->
			{ok, Index, lists:reverse (Outcomes)};
		_ ->
			{ok, Index - 1, lists:reverse (Outcomes)}
	end.

open_port (Name, Settings) ->
	open_port (Name, Settings, 1).

open_port (Name, Settings, Count) when is_integer (Count), Count > 0 ->
	open_port (Name, Settings, Count, Count * 2).

open_port (Name, Settings, Count, Retries) when is_integer (Count), is_integer (Retries), Count > 0, Retries >= Count ->
	Service = mosaic_executor,
	Master = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	Fuzz = erlang:make_ref (),
	{ok, _, {Keys, ErrorReasons}, _} = sync_command (
			fun (Index) ->
				Key = chash:key_of (term_to_binary ({Fuzz, Index})),
				{ok, Key, {open_port, Key, Name, Settings}, 1, Service, Master, Index + 1}
			end, 0,
			fun ({Key, {open_port, Key, _, _}, 1, _, _}, [{_, Reply}], {Keys, ErrorReasons}) ->
				case Reply of
					{open_port_ok, Key} ->
						{ok, {[Key | Keys], ErrorReasons}};
					{error, Reason} ->
						{ok, {Keys, [Reason | ErrorReasons]}};
					_ ->
						{ok, {Keys, [{invalid_outcome, Reply} | ErrorReasons]}}
				end
			end, {[], []},
			Count, Retries),
	{ok, lists:reverse (Keys), lists:reverse (ErrorReasons)}.

sync_command (RequestFun, RequestState, RepliesFun, RepliesState, Count, Retries)
		when is_function (RequestFun, 1), is_function (RepliesFun, 3),
				is_integer (Count), is_integer (Retries), Count >= 0, Retries >= Count ->
	sync_command1 (RequestFun, RequestState, RepliesFun, RepliesState, undefined, Count, Retries).

sync_command1 (_RequestFun, RequestState, _RepliesFun, RepliesState, PendingRequest, 0, _Retries) ->
	{ok, RequestState, RepliesState, PendingRequest};
	
sync_command1 (_RequestFun, RequestState, _RepliesFun, RepliesState, PendingRequest, _Count, 0) ->
	{ok, RequestState, RepliesState, PendingRequest};
	
sync_command1 (RequestFun, OldRequestState, RepliesFun, RepliesState, undefined, Count, Retries) ->
	{ok, Key, Command, Fanout, Service, Master, NewRequestState} = RequestFun (OldRequestState),
	sync_command1 (RequestFun, NewRequestState, RepliesFun, RepliesState, {Key, Command, Fanout, Service, Master}, Count, Retries);
	
sync_command1 (RequestFun, RequestState, RepliesFun, OldRepliesState, PendingRequest = {Key, Command, Fanout, Service, Master}, Count, Retries) ->
	case riak_core_apl:get_apl (Key, Fanout, Service) of
		Targets when is_list (Targets), length (Targets) =:= Fanout ->
			Replies = lists:map (
					fun (Target) -> {Target, riak_core_vnode_master:sync_command (Target, Command, Master)} end,
					Targets),
			{ok, NewRepliesState} = RepliesFun (PendingRequest, Replies, OldRepliesState),
			sync_command1 (RequestFun, RequestState, RepliesFun, NewRepliesState, undefined, Count - 1, Retries);
		[] ->
			sync_command1 (RequestFun, RequestState, RepliesFun, OldRepliesState, PendingRequest, Count, Retries - 1)
	end.
