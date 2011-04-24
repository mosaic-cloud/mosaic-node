
-module (mosaic_dummy_process).

-behaviour (mosaic_process).

-export ([
		init/1, terminate/2, handle_stop/2,
		handle_call/3, handle_cast/2, handle_info/2,
		begin_migration/2, commit_migration/1, rollback_migration/1]).


init ({create, defaults}) ->
	Token = erlang:phash2 (erlang:now (), 1 bsl 32),
	TokenHex = string:to_lower (string:right (erlang:integer_to_list (Token, 16), 8, $0)),
	Arguments = {
			{spawn_executable, "/bin/cat"},
			[{arg0, "[mosaic_dummy_process#" ++ TokenHex ++ "]"}]},
	mosaic_port_process:init ({create, Arguments});
	
init (migrate) ->
	mosaic_port_process:init (migrate);
	
init (_Disposition) ->
	{stop, invalid_disposition}.


terminate (Reason, State) ->
	mosaic_port_process:terminate (Reason, State).

handle_stop (Signal, State) ->
	mosaic_port_process:handle_stop (Signal, State).

handle_call (Request, Sender, State) ->
	mosaic_port_process:handle_call (Request, Sender, State).

handle_cast (Request, State) ->
	mosaic_port_process:handle_cast (Request, State).

handle_info (Message, State) ->
	mosaic_port_process:handle_info (Message, State).

begin_migration (Disposition, State) ->
	mosaic_port_process:begin_migration (Disposition, State).

commit_migration (State) ->
	mosaic_port_process:commit_migration (State).

rollback_migration (State) ->
	mosaic_port_process:rollback_migration (State).
