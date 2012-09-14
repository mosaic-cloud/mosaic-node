
-module (mosaic_cluster_processes).


-export ([resolve/1, resolve/0, define/4, examine/1, examine/0, create/1, stop/1, stop/2]).
-export ([define_and_create/3, define_and_create/4]).
-export ([list/0, map/1]).
-export ([service_nodes/0, service_activate/0, service_deactivate/0, service_ping/0, service_ping/1]).


resolve (Key)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case resolve ([Key]) of
		{ok, [{Key, Process}], []} ->
			{ok, Process};
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end;
	
resolve (Keys)
		when is_list (Keys), (Keys =/= []) ->
	mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key) -> {mosaic_cluster_processes, resolve, Key} end,
			fun (Key, _Request, Reply) ->
				case Reply of
					{ok, Process} when is_pid (Process) ->
						{outcome, {Key, Process}};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys, mosaic_cluster_processes, mosaic_cluster_processes_vnode, 1).

resolve () ->
	case list () of
		Outcome = {ok, [], ListReasons} when is_list (ListReasons) ->
			Outcome;
		{ok, Keys, ListReasons} when is_list (Keys), is_list (ListReasons) ->
			case resolve (Keys) of
				{ok, Processes, ResolveReasons} ->
					{ok, Processes, ListReasons ++ ResolveReasons};
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


examine (Key)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case examine ([Key]) of
		{ok, [{Key, Details}], []} ->
			{ok, Details};
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end;
	
examine (Keys)
		when is_list (Keys), (Keys =/= []) ->
	mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key) -> {mosaic_cluster_processes, examine, Key} end,
			fun (Key, _Request, Reply) ->
				case Reply of
					{ok, Details} ->
						{outcome, {Key, Details}};
					Error = {error, _Reason} ->
						Error;
					_ ->
						{error, {invalid_reply, Reply}}
				end
			end,
			Keys, mosaic_cluster_processes, mosaic_cluster_processes_vnode, 1).

examine () ->
	case list () of
		Outcome = {ok, [], ListReasons} when is_list (ListReasons) ->
			Outcome;
		{ok, Keys, ListReasons} when is_list (Keys), is_list (ListReasons) ->
			case examine (Keys) of
				{ok, Details, ExamineReasons} ->
					{ok, Details, ListReasons ++ ExamineReasons};
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


define (Type, ConfigurationEncoding, ConfigurationContent, Key)
		when is_atom (Type), is_atom (ConfigurationEncoding), is_binary (Key), (bit_size (Key) =:= 160) ->
	case define (Type, ConfigurationEncoding, ConfigurationContent, [Key]) of
		{ok, [Key], []} ->
			ok;
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end;
	
define (Type, ConfigurationEncoding, ConfigurationContent, Keys)
		when is_atom (Type), is_atom (ConfigurationEncoding), is_list (Keys), (Keys =/= []) ->
	mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key) -> {mosaic_cluster_processes, define, Key, Type, ConfigurationEncoding, ConfigurationContent} end,
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
			Keys, mosaic_cluster_processes, mosaic_cluster_processes_vnode, 1).


create (Key)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case create ([Key]) of
		{ok, [Key], []} ->
			ok;
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end;
	
create (Keys)
		when is_list (Keys), (Keys =/= []) ->
	mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key) -> {mosaic_cluster_processes, create, Key} end,
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
			Keys, mosaic_cluster_processes, mosaic_cluster_processes_vnode, 1).


stop (KeyOrKeys) ->
	stop (KeyOrKeys, normal).

stop (Key, Signal)
		when is_binary (Key), (bit_size (Key) =:= 160) ->
	case stop ([Key], Signal) of
		{ok, [Key], []} ->
			ok;
		{ok, [], [{_Target, Key, Reason}]} ->
			{error, Reason};
		{ok, [], []} ->
			{error, unreachable_vnode}
	end;
	
stop (Keys, Signal)
		when is_list (Keys), (Keys =/= []) ->
	mosaic_cluster_tools:service_request_reply_sync_command (
			fun (Key) -> {mosaic_cluster_processes, stop, Key, Signal} end,
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
			Keys, mosaic_cluster_processes, mosaic_cluster_processes_vnode, 1).


define_and_create (Type, ConfigurationEncoding, ConfigurationContent)
		when is_atom (Type), is_atom (ConfigurationEncoding) ->
	{ok, Key} = mosaic_cluster_tools:key (),
	case define (Type, ConfigurationEncoding, ConfigurationContent, Key) of
		ok ->
			case create (Key) of
				ok ->
					case resolve (Key) of
						{ok, Process} ->
							{ok, Key, Process};
						Error = {error, _Reason} ->
							Error
					end;
				Error = {error, _Reason} ->
					Error
			end;
		Error = {error, _Reason} ->
			Error
	end.


define_and_create (Type, ConfigurationEncoding, ConfigurationContent, Count)
		when is_atom (Type), is_atom (ConfigurationEncoding), is_integer (Count), (Count > 0) ->
	{ok, Keys} = mosaic_cluster_tools:keys (Count),
	case define (Type, ConfigurationEncoding, ConfigurationContent, Keys) of
		{ok, [], DefineReasons} ->
			{ok, [], DefineReasons};
		{ok, DefineKeys, DefineReasons} ->
			case create (DefineKeys) of
				{ok, [], CreateReasons} ->
					{ok, [], DefineReasons ++ CreateReasons};
				{ok, CreateKeys, CreateReasons} ->
					case resolve (CreateKeys) of
						{ok, [], ResolveReasons} ->
							{ok, [], DefineReasons ++ CreateReasons ++ ResolveReasons};
						{ok, Processes, ResolveReasons} ->
							{ok, Processes, DefineReasons ++ CreateReasons ++ ResolveReasons}
					end
			end
	end.


list () ->
	mosaic_cluster_tools:service_list_sync_command (mosaic_cluster_processes, mosaic_cluster_processes_vnode).

map (Mapper) ->
	mosaic_cluster_tools:service_map_sync_command (mosaic_cluster_processes, mosaic_cluster_processes_vnode, Mapper).

service_nodes () ->
	mosaic_cluster_tools:service_nodes (mosaic_cluster_processes).

service_activate () ->
	mosaic_cluster_tools:service_activate (mosaic_cluster_processes, mosaic_cluster_processes_vnode).

service_deactivate () ->
	mosaic_cluster_tools:service_deactivate (mosaic_cluster_processes).

service_ping () ->
	service_ping (default).

service_ping (Count) ->
	mosaic_cluster_tools:service_ping (mosaic_cluster_processes, mosaic_cluster_processes_vnode, Count).
