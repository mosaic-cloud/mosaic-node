
-module (mosaic_static_resources_wm).


-export ([
		init/1, ping/2,
		allowed_methods/2,
		resource_exists/2, previously_existed/2, moved_temporarily/2,
		malformed_request/2,
		content_types_provided/2,
		handle_static/2,
		handle_file/2]).


-dispatch ({[], {root}}).
-dispatch ({[<<"static">>, '*'], {static}}).
-dispatch ({[<<"log">>], {log, html}}).
-dispatch ({[<<"log">>, <<"raw">>], {log, raw}}).


-record (state, {target, resource}).


init (Target) ->
	{ok, #state{target = Target, resource = none}}.


ping(Request, State = #state{}) ->
    {pong, Request, State}.


allowed_methods (Request, State = #state{}) ->
	Outcome = {ok, ['GET']},
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


resource_exists (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{root} ->
			{ok, false};
		{log, html} ->
			{ok, false};
		_ ->
			{ok, true}
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


previously_existed (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{root} ->
			{ok, true};
		{log, html} ->
			{ok, true};
		_ ->
			{ok, true}
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


moved_temporarily (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{root} ->
			{ok, {true, "/static/console.html"}};
		{log, html} ->
			{ok, {true, "/static/log.html"}};
		_ ->
			{ok, false}
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


malformed_request (Request, State = #state{target = Target}) ->
	Outcome = case Target of
		{root} ->
			mosaic_webmachine:enforce_get_request ([], Request);
		{log, raw} ->
			mosaic_webmachine:enforce_get_request ([], Request);
		{static} ->
			mosaic_webmachine:enforce_get_request ([], Request)
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, State).


content_types_provided (Request, State = #state{target = {root}}) ->
	Outcome = {ok, [{"application/octet-stream", handle_empty}]},
	mosaic_webmachine:return_with_outcome (Outcome, Request, State);
	
content_types_provided (Request, OldState = #state{target = {log, raw}}) ->
	{Outcome, NewState} = case mosaic_generic_coders:os_env_get (<<"mosaic_node_log">>) of
		{ok, Path} ->
			{{ok, [{"text/plain", handle_file}]}, OldState#state{resource = {Path, <<"text/plain">>}}};
		Error = {error, _Reason} ->
			{Error, OldState}
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, NewState);
	
content_types_provided (Request, OldState = #state{target = {static}}) ->
	Path = erlang:list_to_binary (lists:map (fun (PathToken) -> [$/, PathToken] end, wrq:path_tokens (Request))),
	{Outcome, NewState} = case mosaic_static_resources:contents (Path) of
		{ok, MimeType, Data} ->
			{{ok, [{erlang:binary_to_list (MimeType), handle_static}]}, OldState#state{resource = {Path, MimeType, Data}}};
		Error = {error, _Reason} ->
			{Error, OldState}
	end,
	mosaic_webmachine:return_with_outcome (Outcome, Request, NewState).


handle_static (Request, State = #state{target = {static}, resource = {_Path, MimeType, Data}}) ->
	Outcome = {ok, {mime, MimeType}, Data},
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).


handle_file (Request, State = #state{target = {log, raw}, resource = {Path, MimeType}}) ->
	Outcome = case file:read_file (erlang:binary_to_list (Path)) of
		{ok, Data} ->
			{ok, {mime, MimeType}, Data};
		Error = {error, _Reason} ->
			Error
	end,
	mosaic_webmachine:respond_with_outcome (Outcome, Request, State).
