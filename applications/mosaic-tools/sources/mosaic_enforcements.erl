
-module (mosaic_enforcements).


-export ([enforce/1, enforce/2]).
-export ([enforce_ok/1, enforce_ok_1/1, enforce_ok_2/1]).


enforce (ok) ->
	ok;
	
enforce (Outcome)
		when is_tuple (Outcome), (element (1, Outcome) =:= ok) ->
	Outcome;
	
enforce (Error = {error, _Reason}) ->
	throw (Error);
	
enforce (Outcome) ->
	throw ({error, {invalid_outcome, Outcome}}).


enforce (Outcome, [{Outcome, Replacement} | _]) ->
	enforce (Replacement);
	
enforce (Outcome, [{_, _} | Transformations]) ->
	enforce (Outcome, Transformations);
	
enforce (Outcome, []) ->
	enforce (Outcome).


enforce_ok (ok) ->
	ok;
	
enforce_ok (Error = {error, _Reason}) ->
	throw (Error);
	
enforce_ok (Outcome) ->
	throw ({error, {invalid_outcome, Outcome}}).


enforce_ok_1 ({ok, Value}) ->
	Value;
	
enforce_ok_1 (Error = {error, _Reason}) ->
	throw (Error);
	
enforce_ok_1 (Outcome) ->
	throw ({error, {invalid_outcome, Outcome}}).


enforce_ok_2 ({ok, Value1, Value2}) ->
	{Value1, Value2};
	
enforce_ok_2 (Error = {error, _Reason}) ->
	throw (Error);
	
enforce_ok_2 (Outcome) ->
	throw ({error, {invalid_outcome, Outcome}}).
