
-module (mosaic_object_store).

-behaviour (gen_server).


-export ([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2]).
-export ([start_supervised/1, start_supervised/2]).
-export ([stop/1, stop/2]).
-export ([select/2, include/4, exclude/3, update/5, fold/3, count/1]).
-export ([migrate/3]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start () ->
	start (defaults).

start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	mosaic_tools:start (gen_server, mosaic_object_store, QualifiedName, Configuration).


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_tools:start_link (gen_server, mosaic_object_store, QualifiedName, Configuration).


start_supervised (QualifiedName) ->
	start_supervised (QualifiedName, defaults).

start_supervised (QualifiedName, Configuration) ->
	mosaic_sup:start_child_object_store (QualifiedName, Configuration).


stop (Store) ->
	stop (Store, normal).

stop (Store, Signal)
		when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {mosaic_object_store, stop, Signal}).


select (Store, Key)
		when (is_pid (Store) orelse is_atom (Store)) ->
	gen_server:call (Store, {mosaic_object_store, select, Key}).


include (Store, Key, Revision, Data)
		when (is_pid (Store) orelse is_atom (Store)) ->
	gen_server:call (Store, {mosaic_object_store, include, Key, Revision, Data}).


exclude (Store, Key, Revision)
		when (is_pid (Store) orelse is_atom (Store)) ->
	gen_server:call (Store, {mosaic_object_store, exclude, Key, Revision}).


update (Store, Key, OldRevision, NewRevision, NewData)
		when (is_pid (Store) orelse is_atom (Store)) ->
	gen_server:call (Store, {mosaic_object_store, update, Key, OldRevision, NewRevision, NewData}).


fold (Store, Fun, InputAcc)
		when (is_pid (Store) orelse is_atom (Store)), is_function (Fun, 2) ->
	gen_server:call (Store, {mosaic_object_store, fold, Fun, InputAcc}).


count (Store)
		when (is_pid (Store) orelse is_atom (Store)) ->
	gen_server:call (Store, {mosaic_object_store, count}).


migrate (SourceStore, TargetStore, Key)
		when is_pid (SourceStore), is_pid (TargetStore), (SourceStore =/= TargetStore) ->
	gen_server:call (TargetStore, {mosaic_object_store, migrate_as_target, SourceStore, Key}).


-record (state, {qualified_name, table}).


init ({QualifiedName, defaults}) ->
	false = erlang:process_flag (trap_exit, true),
	case mosaic_tools:ensure_registered (QualifiedName) of
		ok ->
			Table = ets:new (noname, [ordered_set, protected]),
			{ok, #state{qualified_name = QualifiedName, table = Table}};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{table = Table}) ->
	true = ets:delete (Table),
	ok.


code_change (_OldVsn, State, _Arguments) ->
	{ok, State}.


handle_call ({mosaic_object_store, stop, Signal}, _Sender, State) ->
	case Signal of
		normal ->
			{stop, Signal, ok, State};
		_ ->
			{reply, {error, invalid_signal}, State}
	end;
	
handle_call ({mosaic_object_store, select, Key}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[Object = {Key, _Revision, _Data}] ->
			{reply, {ok, Object}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({mosaic_object_store, include, Key, Revision, Data}, _Sender, State = #state{table = Table}) ->
	case ets:insert_new (Table, {Key, Revision, Data}) of
		true ->
			{reply, ok, State};
		false ->
			{reply, {error, already_exists}, State}
	end;
	
handle_call ({mosaic_object_store, update, Key, OldRevision, NewRevision, NewData}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[{Key, OldRevision, _OldData}] ->
			true = ets:insert (Table, {Key, NewRevision, NewData}),
			{reply, ok, State};
		[{Key, OtherOldRevision, _OldData}] ->
			{reply, {error, {mismatched_revision, OtherOldRevision}}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({mosaic_object_store, exclude, Key, Revision}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[{Key, Revision, _Data}] ->
			true = ets:delete (Table, Key),
			{reply, ok, State};
		[{Key, OtherRevision, _Data}] ->
			{reply, {error, {mismatched_revision, OtherRevision}}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({mosaic_object_store, fold, Function, InputAccumulator}, _Sender, State = #state{table = Table})
		when is_function (Function, 2) ->
	OutputAccumulator = ets:foldl (Function, InputAccumulator, Table),
	{reply, {ok, OutputAccumulator}, State};
	
handle_call ({mosaic_object_store, count}, _Sender, State = #state{table = Table}) ->
	TableInfo = ets:info (Table),
	Count = proplists:get_value (size, TableInfo),
	{reply, {ok, Count}, State};
	
handle_call ({mosaic_object_store, migrate_as_target, SourceStore, Key}, _Sender, State = #state{table = Table})
		when is_pid (SourceStore) ->
	case gen_server:call (SourceStore, {mosaic_object_store, migrate_as_source, Key}) of
		{ok, SourceRevision, SourceData} ->
			case ets:lookup (Table, Key) of
				[{Key, SourceRevision, SourceData}] ->
					{reply, ok, State};
				[{Key, SourceRevision, _TargetData}] ->
					% !!!!
					{reply, ok, State};
				[{Key, _TargetRevision, _TargetData}] ->
					% !!!!
					{reply, ok, State};
				[] ->
					true = ets:insert (Table, {Key, SourceRevision, SourceData}),
					{reply, ok, State}
			end;
		Error = {error, _Reason} ->
			{reply, Error, State}
	end;
	
handle_call ({mosaic_object_store, migrate_as_source, Key}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[{Key, Revision, Data}] ->
			true = ets:delete (Table, Key),
			{reply, {ok, Revision, Data}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call (Request, Sender, State) ->
	ok = mosaic_tools:trace_error ("received invalid call request; ignoring!", [{request, Request}, {sender, Sender}]),
	{reply, {error, {invalid_request, Request}}, State}.


handle_cast (Request, State) ->
	ok = mosaic_tools:trace_error ("received invalid cast request; ignoring!", [{request, Request}]),
	{noreply, State}.


handle_info (Message, State) ->
	ok = mosaic_tools:trace_error ("received invalid message; ignoring!", [{message, Message}]),
	{noreply, State}.
