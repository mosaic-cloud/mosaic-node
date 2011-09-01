
-module (mosaic_component_process_coders).


-export ([validate_configuration/2, parse_configuration/3]).


-include ("mosaic_component_process.hrl").


validate_configuration (
			Disposition,
			#configuration{harness = HarnessOptions, execute = ExecuteSpecification, resources = Resources, router = Router})
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	try
		ok = if
			is_list (HarnessOptions) ->
				ok;
			true ->
				throw ({error, {invalid_harness, HarnessOptions}})
		end,
		ok = if
			(Disposition =:= create) ->
				ok = case mosaic_harness_coders:validate_frontend_execute_specification (ExecuteSpecification) of
					ok ->
						ok;
					Error1 = {error, _Reason1} ->
						throw (Error1)
				end;
			(Disposition =:= migrate), (ExecuteSpecification =:= migrate) ->
				ok;
			true ->
				throw ({error, {invalid_execute, ExecuteSpecification}})
		end,
		ok = if
			(is_pid (Resources) orelse is_atom (Resources)) ->
				ok;
			true ->
				throw ({error, {invalid_resources, Resources}})
		end,
		ok = if
			(is_pid (Router) orelse is_atom (Router)) ->
				ok;
			true ->
				throw ({error, {invalid_router, Router}})
		end,
		ok
	catch
		throw : Error = {error, _Reason} ->
			Error
	end;
	
validate_configuration (Disposition, Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	{error, {invalid_configuration, Configuration}};
	
validate_configuration (Disposition, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.


parse_configuration (Disposition, term, OriginalOptions)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)), is_list (OriginalOptions) ->
	DefaultOptions = [{harness, defaults}, {execute, undefined}, {resources, defaults}, {router, defaults}],
	FinalOptions = OriginalOptions ++ DefaultOptions,
	case lists:sort (proplists:get_keys (FinalOptions)) of
		[execute, harness, resources, router] ->
			try
				{ok, HarnessOptions} = case proplists:get_value (harness, FinalOptions) of
					undefined ->
						throw ({error, missing_harness});
					defaults ->
						{ok, []};
					HarnessOptions_ when is_list (HarnessOptions_) ->
						{ok, HarnessOptions_};
					HarnessOptions_ ->
						throw ({error, {invalid_harness, HarnessOptions_}})
				end,
				{ok, ExecuteSpecification} = case proplists:get_value (execute, FinalOptions) of
					undefined when (Disposition =:= create) ->
						throw ({error, missing_execute});
					undefined when (Disposition =:= migrate) ->
						{ok, migrate};
					ExecuteSpecification__ when (Disposition =:= create) ->
						case mosaic_harness_coders:decode_frontend_execute_specification (term, ExecuteSpecification__) of
							{ok, ExecuteSpecification_} ->
								{ok, ExecuteSpecification_};
							{error, Reason1} ->
								throw ({error, {invalid_execute, Reason1}})
						end;
					ExecuteSpecification__ when (Disposition =:= migrate) ->
						throw ({error, {unexpected_execute, ExecuteSpecification__}})
				end,
				{ok, Resources} = case proplists:get_value (resources, FinalOptions) of
					undefined ->
						throw ({error, missing_resources});
					defaults ->
						{ok, mosaic_component_resources};
					Resources_ when (is_pid (Resources_) orelse is_atom (Resources_)) ->
						{ok, Resources_};
					Resources_ ->
						throw ({error, {invalid_resources, Resources_}})
				end,
				{ok, Router} = case proplists:get_value (router, FinalOptions) of
					undefined ->
						throw ({error, missing_router});
					defaults ->
						{ok, mosaic_process_router};
					Router_ when (is_pid (Router_) orelse is_atom (Router_)) ->
						{ok, Router_};
					Router_ ->
						throw ({error, {invalid_router, Router_}})
				end,
				Configuration = #configuration{
						harness = HarnessOptions,
						execute = ExecuteSpecification,
						resources = Resources,
						router = Router},
				{ok, Configuration}
			catch
				throw : Error = {error, _Reason} ->
					Error
			end;
		_ ->
			{error, {invalid_configuration, OriginalOptions}}
	end;
	
parse_configuration (Disposition, term, Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	case validate_configuration (Disposition, Configuration) of
		ok ->
			{ok, Configuration};
		Error = {error, _Reason} ->
			Error
	end;
	
parse_configuration (Disposition, Encoding, _Configuration)
		when ((Disposition =:= create) orelse (Disposition =:= migrate)) ->
	{error, {invalid_encoding, Encoding}};
	
parse_configuration (Disposition, _Encoding, _Configuration) ->
	{error, {invalid_disposition, Disposition}}.
