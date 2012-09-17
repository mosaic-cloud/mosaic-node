
-module (mosaic_cluster_component_transcript).

-behaviour (gen_server).


-export ([start_supervised/0, start_supervised/1, start_link/2]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1]).


start_supervised () ->
	start_supervised (defaults).

start_supervised (Configuration) ->
	mosaic_node_sup:start_child_daemon ({local, mosaic_component_transcript}, mosaic_cluster_component_transcript, Configuration).

start_link (QualifiedName, Configuration) ->
	mosaic_process_tools:start_link (gen_server, mosaic_cluster_component_transcript, QualifiedName, Configuration).


-record (state, {qualified_name, table}).


init ({QualifiedName, defaults}) ->
	case mosaic_process_tools:ensure_registered (QualifiedName) of
		ok ->
			Table = ets:new (mosaic_cluster_component_transcript, [ordered_set, protected, named_table]),
			State = #state{qualified_name = QualifiedName, table = Table},
			{ok, State};
		{error, Reason} ->
			{stop, Reason}
	end.


terminate (_Reason, _State = #state{}) ->
	ok.


code_change (_OldVsn, State = #state{}, _Arguments) ->
	{ok, State}.


handle_call (
			{mosaic_component_transcript, push, Identifier, Data}, _Sender,
			State = #state{table = Table})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160), is_binary (Data) ->
	try
		enforce_ok (execute_push (Identifier, Data, Table)),
		{reply, ok, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call (
			{mosaic_component_transcript, select, Identifier}, Sender,
			State = #state{})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	erlang:spawn (fun () ->
		Outcome = try
			Records = enforce_ok_1 (execute_select_global (Identifier)),
			{ok, Records}
		catch throw : Error = {error, _Reason} -> Error end,
		_ = gen_server:reply (Sender, Outcome),
		erlang:exit (normal)
	end),
	{noreply, State};
	
handle_call (
			{mosaic_component_transcript, select}, Sender,
			State = #state{}) ->
	erlang:spawn (fun () ->
		Outcome = try
			Records = enforce_ok_1 (execute_select_global ()),
			{ok, Records}
		catch throw : Error = {error, _Reason} -> Error end,
		_ = gen_server:reply (Sender, Outcome),
		erlang:exit (normal)
	end),
	{noreply, State};
	
handle_call (
			{mosaic_component_transcript, select_local, Identifier}, _Sender,
			State = #state{table = Table})
		when is_binary (Identifier), (bit_size (Identifier) =:= 160) ->
	try
		Records = enforce_ok_1 (execute_select_local (Identifier, Table)),
		{reply, {ok, Records}, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call (
			{mosaic_component_transcript, select_local}, _Sender,
			State = #state{table = Table}) ->
	try
		Records = enforce_ok_1 (execute_select_local (Table)),
		{reply, {ok, Records}, State}
	catch throw : Error = {error, _Reason} -> {reply, Error, State} end;
	
handle_call (Request, _Sender, State = #state{}) ->
	Error = {error, {invalid_request, Request}},
	{stop, Error, Error, State}.


handle_cast (Request, State = #state{}) ->
	{stop, {error, {invalid_request, Request}}, State}.


handle_info (Message, State = #state{}) ->
	{stop, {error, {invalid_message, Message}}, State}.


execute_push (Identifier, Data, Table) ->
	Timestamp = erlang:now (),
	true = ets:insert (Table, {{Identifier, Timestamp}, Data}),
	IdentifierBinary = enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
	<<IdentifierPart : 16 / binary, _ / binary>> = IdentifierBinary,
	_ = io:put_chars (standard_error, [
			<<"[">>, IdentifierPart, <<"] ">>,
			Data]),
	ok.


execute_select_global () ->
	execute_select_global_ ({mosaic_component_transcript, select_local}).

execute_select_global (Identifier) ->
	execute_select_global_ ({mosaic_component_transcript, select_local, Identifier}).

execute_select_global_ (Request) ->
	Nodes = enforce_ok_1 (mosaic_cluster_tools:ring_nodes ()),
	{Replies, _FailedNodes} = gen_server:multi_call (Nodes, mosaic_component_transcript, Request),
	Records = lists:foldl (
			fun
				({_Node, {ok, Records}}, CollectedRecords) ->
					Records ++ CollectedRecords;
				({_Node, {error, _Reason}}, CollectedRecords) ->
					CollectedRecords
			end,
			[], Replies),
	{ok, Records}.


execute_select_local (Identifier, Table) ->
	execute_select_local ({Identifier, {0, 0, 0}}, Table, []).

execute_select_local (PreviousKey = {Identifier, _PreviousTimestamp}, Table, PreviousRecords) ->
	case ets:next (Table, PreviousKey) of
		CurrentKey = {Identifier, CurrentTimestamp} ->
			case ets:lookup (Table, CurrentKey) of
				[{CurrentKey, CurrentData}] ->
					execute_select_local (CurrentKey, Table, [{CurrentTimestamp, CurrentData} | PreviousRecords]);
				[] ->
					execute_select_local (CurrentKey, Table, PreviousRecords)
			end;
		{OtherIdentifier, _OtherTimestamp} when (OtherIdentifier =/= Identifier) ->
			{ok, PreviousRecords};
		'$end_of_table' ->
			{ok, PreviousRecords}
	end.


execute_select_local (Table) ->
	{ok, ets:foldl (
			fun ({{Identifier, Timestamp}, Data}, PreviousRecords) ->
				[{Identifier, Timestamp, Data} | PreviousRecords]
			end, [], Table)}.
