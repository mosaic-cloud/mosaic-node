
-module (mosaic_executor_vnode).

-behaviour (riak_core_vnode).

-export ([start/0]).
-export ([
		start_vnode/1,
		init/1, terminate/2, delete/1, is_empty/1,
		handle_command/3,
		handoff_starting/2, handoff_finished/2, handoff_cancelled/1,
		handle_handoff_command/3, handle_handoff_data/2,
		encode_handoff_item/2]).

-record (state, {partition, ports}).

start () ->
	MasterName = riak_core_vnode_master:reg_name (mosaic_executor_vnode),
	MasterSpecification = {MasterName, {riak_core_vnode_master, start_link, [mosaic_executor_vnode]}, permanent, 60, worker, dynamic},
	MasterOutcome = case supervisor:start_child (mosaic_cluster_sup, MasterSpecification) of
		{ok, MasterProcess1} ->
			{ok, MasterProcess1};
		{error, {already_started, MasterProcess1}} ->
			{ok, MasterProcess1};
		Error1 = {error, _Reason1} ->
			Error1
	end,
	case MasterOutcome of
		{ok, MasterProcess2} ->
			ok = riak_core:register_vnode_module (mosaic_executor_vnode),
			ok = riak_core_node_watcher:service_up (mosaic_executor, MasterProcess2),
			ok;
		Error2 = {error, _Reason2} ->
			Error2
	end.

start_vnode (Index) ->
	riak_core_vnode_master:get_vnode_pid (Index, mosaic_executor_vnode).

init ([Partition]) ->
	ok = error_logger:info_report ([{?MODULE, init, Partition, erlang:self ()}]),
	{ok, #state{partition = Partition, ports = orddict:new ()}}.

terminate (_Reason, _State) ->
	ok.

delete (State) ->
	{ok, State}.

is_empty (State) ->
	{false, State}.

handle_command ({ping, Key}, _Sender, State) ->
	{reply, {pong, Key, {State#state.partition, erlang:node ()}}, State};
	
handle_command ({open_port, Key, Name, Settings}, _Sender, OldState = #state{ports = OldPorts}) ->
	ok = error_logger:info_report ([{?MODULE, open_port, OldState#state.partition, erlang:self (), Key, Name, Settings}]),
	Port = erlang:open_port (Name, Settings),
	NewPorts = orddict:store (Key, {Port, Name, Settings}, OldPorts),
	NewState = OldState#state{ports = NewPorts},
	{reply, {open_port_ok, Key}, NewState};
	
handle_command ({riak_core_fold_req_v1, Fun, InputAcc}, _Sender, State = #state{ports = Ports}) ->
	OutputAcc = orddict:fold (
			fun (Key, {_Port, Name, Settings}, CurrentAcc) -> Fun (Key, {Name, Settings}, CurrentAcc) end,
			InputAcc, Ports),
	{reply, OutputAcc, State};
	
handle_command (Command, _Sender, State) ->
	{reply, {error, {invalid_command, Command}}, State}.

handoff_starting (Node, State) ->
	ok = error_logger:info_report ([{?MODULE, handoff_starting, State#state.partition, erlang:self (), Node}]),
	{true, State}.

handoff_finished (Node, State) ->
	ok = error_logger:info_report ([{?MODULE, handoff_finished, State#state.partition, erlang:self (), Node}]),
	ok.

handoff_cancelled (State) ->
	ok = error_logger:info_report ([{?MODULE, handoff_canceled, State#state.partition, erlang:self ()}]),
	{ok, State}.

handle_handoff_command (Request = {riak_core_fold_req_v1, _Fun, _Acc}, Sender, State) ->
	handle_command (Request, Sender, State);
	
handle_handoff_command (_Request, _Sender, State) ->
	{forward, State}.

handle_handoff_data (Binary, OldState = #state{ports = OldPorts}) ->
	Object = {Key, {Name, Settings}} = erlang:binary_to_term (Binary),
	ok = error_logger:info_report ([{?MODULE, handle_handoff_data, OldState#state.partition, erlang:self (), Object}]),
	NewPorts = orddict:store (Key, {undefined, Name, Settings}, OldPorts),
	NewState = OldState#state{ports = NewPorts},
	{reply, ok, NewState}.

encode_handoff_item (Key, Object) ->
	erlang:term_to_binary (Object).
