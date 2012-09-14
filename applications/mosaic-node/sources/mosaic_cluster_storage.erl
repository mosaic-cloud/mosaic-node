
-module (mosaic_cluster_storage).


-export ([select/1, include/3, exclude/2, update/2, update/4]).
-export ([list/0, map/1]).
-export ([service_nodes/0, service_activate/0, service_deactivate/0, service_ping/0, service_ping/1]).


select (Key)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case select ([Key]) of
		{ok, [{Key, Revision, Data} | _Outcomes], _Reasons} ->
			{ok, Revision, Data};
		{ok, [], [{_Target, Key, Reason} | _Reasons]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end;
	
select (Keys)
		when is_list (Keys), (Keys =/= []) ->
	mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key) -> {mosaic_cluster_storage, select, Key} end,
			fun (Key, _Request, Reply) ->
				case Reply of
					{ok, Revision, Data} ->
						{outcome, {Key, Revision, Data}};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys, mosaic_cluster_storage, mosaic_cluster_storage_vnode, 4).


include (Key, Revision, Data)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	Outcome = mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key_) -> {mosaic_cluster_storage, include, Key_, Revision, Data} end,
			fun (Key_, _Request, Reply) ->
				case Reply of
					ok ->
						{outcome, Key_};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			[Key], mosaic_cluster_storage, mosaic_cluster_storage_vnode, 4),
	case Outcome of
		{ok, _Keys, []} ->
			ok;
		{ok, [], [{_Target, Key, Reason} | _Reasons]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end.


exclude (Key, Revision)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	Outcome = mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key_) -> {mosaic_cluster_storage, exclude, Key_, Revision} end,
			fun (Key_, _Request, Reply) ->
				case Reply of
					ok ->
						{outcome, Key_};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			[Key], mosaic_cluster_storage, mosaic_cluster_storage_vnode, 4),
	case Outcome of
		{ok, _Keys, []} ->
			ok;
		{ok, _Keys, [{_Target, Key, Reason} | _Reasons]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end.


update (Key, OldRevision, NewRevision, NewData)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	Outcome = mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key_) -> {mosaic_cluster_storage, update, Key_, OldRevision, NewRevision, NewData} end,
			fun (Key_, _Request, Reply) ->
				case Reply of
					ok ->
						{outcome, Key_};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			[Key], mosaic_cluster_storage, mosaic_cluster_storage_vnode, 4),
	case Outcome of
		{ok, _Keys, []} ->
			ok;
		{ok, _Keys, [{_Target, Key, Reason} | _Reasons]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end.


update (Key, Mutator)
		when is_binary (Key), (bit_size (Key) =:= 160), is_function (Mutator, 1) ->
	Outcome = mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key_) -> {mosaic_cluster_storage, update, Key_, Mutator} end,
			fun (Key_, _Request, Reply) ->
				case Reply of
					ok ->
						{outcome, Key_};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			[Key], mosaic_cluster_storage, mosaic_cluster_storage_vnode, 4),
	case Outcome of
		{ok, _Keys, []} ->
			ok;
		{ok, _Keys, [{_Target, Key, Reason} | _Reasons]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end.


list () ->
	mosaic_cluster_tools:service_list_sync_command (mosaic_cluster_storage, mosaic_cluster_storage_vnode).

map (Mapper) ->
	mosaic_cluster_tools:service_map_sync_command (mosaic_cluster_storage, mosaic_cluster_storage_vnode, Mapper).

service_nodes () ->
	mosaic_cluster_tools:service_nodes (mosaic_cluster_storage).

service_activate () ->
	mosaic_cluster_tools:service_activate (mosaic_cluster_storage, mosaic_cluster_storage_vnode).

service_deactivate () ->
	mosaic_cluster_tools:service_deactivate (mosaic_cluster_storage).

service_ping () ->
	service_ping (default).

service_ping (Count) ->
	mosaic_cluster_tools:service_ping (mosaic_cluster_storage, mosaic_cluster_storage_vnode, Count).
