
-module (mosaic_transcript).


-export ([trace_error/1, trace_error/2, trace_warning/1, trace_warning/2, trace_information/1, trace_information/2]).
-export ([trace_debugging/1, trace_debugging/2]).


trace_debugging (Message) ->
	trace_debugging (Message, []).

trace_debugging (Message, Report) ->
	trace (debugging, Message, Report).

trace_information (Message) ->
	trace_information (Message, []).

trace_information (Message, Report) ->
	trace (information, Message, Report).

trace_warning (Message) ->
	trace_warning (Message, []).

trace_warning (Message, Report) ->
	trace (warning, Message, Report).

trace_error (Message) ->
	trace_error (Message, []).

trace_error (Message, Report) ->
	trace (error, Message, Report).


trace (Level, MessageSpecification, Report)
		when is_tuple (MessageSpecification), (tuple_size (MessageSpecification) > 0), is_list (element (1, MessageSpecification)) ->
	[MessageFormat | MessageArguments] = erlang:tuple_to_list (MessageSpecification),
	Message = lists:flatten (io_lib:format (MessageFormat, MessageArguments)),
	trace (Level, Message, Report);
	
trace (Level, Message, Report)
		when is_list (Message), is_list (Report) ->
	Self = erlang:self (),
	{ok, [_ | StackTrace]} = try throw (stacktrace) catch throw : stacktrace -> {ok, erlang:get_stacktrace ()} end,
	trace (Level, Self, StackTrace, Message, Report).


trace (Level, Process, [LastStackTrace | _], Message, Report) ->
	ok = case Level of
		debugging ->
			ok = error_logger:info_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok;
		information ->
			ok = error_logger:info_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok;
		warning ->
			ok = error_logger:warning_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok;
		error ->
			ok = error_logger:error_report ([Message, {source, Process, LastStackTrace, Level} | Report]),
			ok
	end,
	ok.
