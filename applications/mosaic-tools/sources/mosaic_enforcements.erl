
-module (mosaic_enforcements).


-export ([enforce/1, enforce/2]).
-export ([enforce_ok/1, enforce_ok_1/1, enforce_ok_2/1, enforce_ok_3/1]).


enforce (ok) ->
	ok;
	
enforce (Outcome)
		when is_tuple (Outcome), (element (1, Outcome) =:= ok) ->
	Outcome;
	
enforce (Error = {error, _Reason}) ->
	throw (Error);
	
enforce (Return) ->
	throw ({error, {invalid_return, Return}}).


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
	
enforce_ok (Return) ->
	throw ({error, {invalid_return, Return}}).


enforce_ok_1 ({ok, Value}) ->
	Value;
	
enforce_ok_1 (Error = {error, _Reason}) ->
	throw (Error);
	
enforce_ok_1 (Return) ->
	throw ({error, {invalid_return, Return}}).


enforce_ok_2 ({ok, Value1, Value2}) ->
	{Value1, Value2};
	
enforce_ok_2 (Error = {error, _Reason}) ->
	throw (Error);
	
enforce_ok_2 (Return) ->
	throw ({error, {invalid_return, Return}}).

enforce_ok_3 ({ok, Value1, Value2, Value3}) ->
	{Value1, Value2, Value3};
	
enforce_ok_3 (Error = {error, _Reason}) ->
	throw (Error);
	
enforce_ok_3 (Return) ->
	throw ({error, {invalid_return, Return}}).
