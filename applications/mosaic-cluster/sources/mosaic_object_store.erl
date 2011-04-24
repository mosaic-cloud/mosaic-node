
-module (mosaic_object_store).

-behaviour (gen_server).

-export ([start/2, start_link/2, start_supervised/2]).
-export ([stop/1, stop/2]).
-export ([select/2, include/4, replace/5, exclude/3, fold/3, count/1]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start (QualifiedName = {local, LocalName}, Configuration) when is_atom (LocalName) ->
	gen_server:start (QualifiedName, mosaic_object_store, {QualifiedName, Configuration}, []).

start_link (QualifiedName = {local, LocalName}, Configuration) when is_atom (LocalName) ->
	gen_server:start_link (QualifiedName, mosaic_object_store, {QualifiedName, Configuration}, []).

start_supervised (QualifiedName, Configuration) ->
	mosaic_cluster_sup:start_child_object_store (QualifiedName, Configuration).

stop (Store) ->
	stop (Store, normal).

stop (Store, Signal) when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {stop, Signal}).

select (Store, Key) when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {select, Key}).

include (Store, Key, Revision, Data) when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {include, Key, Revision, Data}).

replace (Store, Key, OldRevision, NewRevision, NewData) when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {replace, Key, OldRevision, NewRevision, NewData}).

exclude (Store, Key, Revision) when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {exclude, Key, Revision}).

fold (Store, Fun, InputAcc) when is_pid (Store) or is_atom (Store), is_function (Fun, 2) ->
	gen_server:call (Store, {fold, Fun, InputAcc}).

count (Store) when is_pid (Store) or is_atom (Store) ->
	gen_server:call (Store, {count}).

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

code_change (_OldVsn, State, _Data) ->
	{ok, State}.

handle_call ({stop, Signal}, _Sender, State) ->
	case Signal of
		normal ->
			{stop, Signal, ok, State};
		_ ->
			{reply, {error, invalid_signal}, State}
	end;
	
handle_call ({select, Key}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[Object = {Key, _Revision, _Data}] ->
			{reply, {ok, Object}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({include, Key, Revision, Data}, _Sender, State = #state{table = Table}) ->
	case ets:insert_new (Table, {Key, Revision, Data}) of
		true ->
			{reply, ok, State};
		false ->
			{reply, {error, already_exists}, State}
	end;
	
handle_call ({replace, Key, OldRevision, NewRevision, NewData}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[{Key, OldRevision, _OldData}] ->
			true = ets:insert (Table, {Key, NewRevision, NewData}),
			{reply, ok, State};
		[{Key, OtherOldRevision, _OldData}] ->
			{reply, {error, {mismatched_revision, OtherOldRevision}}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({exclude, Key, Revision}, _Sender, State = #state{table = Table}) ->
	case ets:lookup (Table, Key) of
		[{Key, Revision, _Data}] ->
			true = ets:delete (Table, Key),
			{reply, ok, State};
		[{Key, OtherRevision, _Data}] ->
			{reply, {error, {mismatched_revision, OtherRevision}}, State};
		[] ->
			{reply, {error, does_not_exist}, State}
	end;
	
handle_call ({fold, Fun, InputAcc}, _Sender, State = #state{table = Table}) when is_function (Fun, 2) ->
	OutputAcc = ets:foldl (Fun, InputAcc, Table),
	{reply, {ok, OutputAcc}, State};
	
handle_call ({count}, _Sender, State = #state{table = Table}) ->
	TableInfo = ets:info (Table),
	Count = proplists:get_value (size, TableInfo),
	{reply, {ok, Count}, State};
	
handle_call (Request, Sender, State) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_object_store, handle_call, erlang:self ()}, {invalid_call, Request, Sender}]),
	{reply, {error, {invalid_call, Request}}, State}.

handle_cast (Request, State) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_object_store, handle_cast, erlang:self ()}, {invalid_cast, Request}]),
	{noreply, State}.

handle_info (Request, State) ->
	ok = error_logger:error_report (mosaic_error, [{mosaic_object_store, handle_info, erlang:self ()}, {invalid_info, Request}]),
	{noreply, State}.
