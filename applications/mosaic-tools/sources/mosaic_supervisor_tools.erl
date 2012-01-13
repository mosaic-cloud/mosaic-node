
-module (mosaic_supervisor_tools).


-export ([
		start/1, start/2,
		start_link/1, start_link/2]).
-export ([
		start_child_process/3, start_child_process/4, start_child_process/5,
		start_link_child_process/3, start_link_child_process/4, start_link_child_process/5]).
-export ([
		start_child/2,
		start_link_child/2]).


-import (mosaic_enforcements, [enforce_ok_1/1]).


start (Configuration) ->
	start (noname, Configuration).

start (QualifiedName, Configuration) ->
	Self = erlang:self (),
	Token = erlang:make_ref (),
	Delegate = erlang:spawn_link (
			fun () ->
				Self ! {Token, start_link (QualifiedName, Configuration)}
			end),
	Monitor = erlang:monitor (process, Delegate),
	receive
		{Token, Outcome} ->
			true = erlang:demonitor (Monitor, [flush]),
			Outcome;
		{'DOWN', Monitor, process, Delegate, Reason} ->
			{error, Reason}
	end.


start_link (Configuration) ->
	start_link (noname, Configuration).

start_link (QualifiedName, Configuration)
		when ((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	try enforce_ok_1 (coerce_configuration (Configuration))
	of CoercedConfiguration -> start_link (QualifiedName, mosaic_supervisor_tools_proxy, [{supervisor, QualifiedName, CoercedConfiguration}])
	catch throw : Error = {error, _Reason} -> Error end.


start_link (QualifiedName, Module, Arguments)
		when is_atom (Module), is_list (Arguments),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	try
		case QualifiedName of
			{local, _LocalName} ->
				supervisor:start_link (QualifiedName, Module, Arguments);
			noname ->
				supervisor:start_link (Module, Arguments)
		end
	of
		Outcome = {ok, Supervisor} when is_pid (Supervisor) ->
			Outcome;
		{error, {already_started, Supervisor}} when is_pid (Supervisor) ->
			{ok, Supervisor};
		Error = {error, _Reason} ->
			Error;
		ignore ->
			{error, ignore};
		Outcome ->
			{error, {invalid_outcome, Outcome}}
	catch
		throw : Error = {error, _Reason} -> Error;
		_ : Reason -> {error, Reason}
	end.


start_child_process (Supervisor, Module, Configuration) ->
	start_child_process (Supervisor, noname, Module, Configuration).

start_child_process (Supervisor, QualifiedName, Module, Configuration) ->
	start_child_process (Supervisor, QualifiedName, Module, Configuration, []).

start_child_process (Supervisor, QualifiedName, Module, Configuration, Options)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)), is_atom (Module), is_list (Options),
				((QualifiedName =:= noname) orelse (is_record (QualifiedName, local, 2) andalso is_atom (element (2, QualifiedName)))) ->
	try
		Link_ = case proplists:lookup (link, Options) of
			{link, true} -> true;
			{link, false} -> false;
			none -> false;
			LinkOption when is_tuple (LinkOption) -> throw ({error, {invalid_options, Options, {invalid_option, LinkOption, invalid_term}}})
		end,
		Specification_ = enforce_ok_1 (coerce_child_specification ({process, QualifiedName, Module, Configuration, Options})),
		{Link_, Specification_}
	of
		{false, Specification} ->
			start_child (Supervisor, Specification);
		{true, Specification} ->
			start_link_child (Supervisor, Specification)
	catch throw : Error = {error, _Reason} -> Error end.


start_link_child_process (Supervisor, Module, Configuration) ->
	start_link_child_process (Supervisor, noname, Module, Configuration).

start_link_child_process (Supervisor, QualifiedName, Module, Configuration) ->
	start_link_child_process (Supervisor, QualifiedName, Module, Configuration, []).

start_link_child_process (Supervisor, QualifiedName, Module, Configuration, Options)
		when is_list (Options) ->
	start_child_process (Supervisor, QualifiedName, Module, Configuration, [{link, true} | Options]).


start_child (Supervisor, OriginalChildSpecification)
		when (is_pid (Supervisor) orelse is_atom (Supervisor)) ->
	try
		ChildSpecification = enforce_ok_1 (coerce_child_specification (OriginalChildSpecification)),
		supervisor:start_child (Supervisor, ChildSpecification)
	of
		Outcome = {ok, Child} when is_pid (Child) ->
			Outcome;
		{ok, Child, _Extra} when is_pid (Child) ->
			{ok, Child};
		{error, {already_started, Child}} when is_pid (Child) ->
			{ok, Child};
		Error = {error, already_present} ->
			Error;
		{error, {Error = {error, _Reason}, _Extra}} ->
			Error;
		Error = {error, _Reason} ->
			Error;
		ignore ->
			{error, ignore};
		Outcome ->
			{error, {invalid_outcome, Outcome}}
	catch
		throw : Error = {error, _Reason} -> Error;
		_ : Reason -> {error, Reason}
	end.

start_link_child (Supervisor, ChildSpecification) ->
	case start_child (Supervisor, ChildSpecification) of
		Outcome = {ok, Child} ->
			true = erlang:link (Child),
			Outcome;
		Error = {error, _Reason} ->
			Error
	end.


coerce_configuration (Configuration = {RestartPolicy, ChildSpecifications}) ->
	try
		{ok, {enforce_ok_1 (coerce_restart_policy (RestartPolicy)), enforce_ok_1 (coerce_child_specifications (ChildSpecifications))}}
	catch throw : {error, Reason} -> {error, {invalid_configuration, Configuration, Reason}} end;
	
coerce_configuration (Configuration) ->
	{error, {invalid_configuration, Configuration, invalid_term}}.


coerce_restart_policy (Strategy)
		when ((Strategy =:= one_for_one) orelse (Strategy =:= one_for_all) orelse (Strategy =:= rest_for_one) orelse (Strategy =:= simple_one_for_one)) ->
	coerce_restart_policy ({Strategy, 10, 3600});
	
coerce_restart_policy (Policy = {Strategy, Count, Interval}) ->
	try
		ok = if
			((Strategy =:= one_for_one) orelse (Strategy =:= one_for_all) orelse (Strategy =:= rest_for_one) orelse (Strategy =:= simple_one_for_one)) -> ok;
			true -> throw ({error, {invalid_strategy, Strategy}})
		end,
		ok = if
			(is_integer (Count) andalso (Count >= 0)) -> ok;
			true -> throw ({error, {invalid_count, Count}})
		end,
		ok = if
			(is_integer (Interval) andalso (Interval >= 0)) -> ok;
			true -> throw ({error, {invalid_interval, Interval}})
		end,
		{ok, Policy}
	catch throw : {error, Reason} -> {error, {invalid_restart_policy, Policy, Reason}} end;
	
coerce_restart_policy (Policy) ->
	{error, {invalid_restart_policy, Policy, invalid_term}}.


coerce_child_specifications (Specifications)
		when is_list (Specifications) ->
	try
		{ok, lists:map (
				fun (Specification) ->
					enforce_ok_1 (coerce_child_specification (Specification))
				end,
				Specifications)}
	catch throw : {error, Reason} -> {error, {invalid_child_specifications, Specifications, Reason}} end;
	
coerce_child_specifications (Specifications) ->
	{error, {invalid_child_specifications, Specifications, invalid_term}}.


coerce_child_specification ({process, Module, Configuration}) ->
	coerce_child_specification ({process, noname, Module, Configuration});
	
coerce_child_specification ({process, QualifiedName, Module, Configuration}) ->
	coerce_child_specification ({process, QualifiedName, Module, Configuration, []});
	
coerce_child_specification (OriginalSpecification = {process, QualifiedName, Module, Configuration, Options}) ->
	try
		ok = if
			is_list (Options) -> ok;
			true -> throw ({error, {invalid_options, Options, invalid_term}})
		end,
		DefaultOptions = [{identifier, undefined}, {restart, temporary}, {shutdown, 12 * 1000}],
		FinalOptions = Options ++ DefaultOptions,
		{Identifier1, Restart, Shutdown} = case lists:sort (proplists:get_keys (FinalOptions)) of
			[identifier, restart, shutdown] ->
				{
					proplists:get_value (identifier, FinalOptions),
					proplists:get_value (restart, FinalOptions),
					proplists:get_value (shutdown, FinalOptions)};
			OptionKeys ->
				throw ({error, {invalid_options, Options, {invalid_option_keys, lists:subtract (OptionKeys, proplists:get_keys (DefaultOptions))}}})
		end,
		StartLinkArguments = if
			(QualifiedName =:= noname) -> [Configuration];
			true -> [QualifiedName, Configuration]
		end,
		Identifier2 = if
			(Identifier1 =/= undefined) -> Identifier1;
			(QualifiedName =/= noname) ->
				case QualifiedName of
					{local, LocalName} -> LocalName;
					{global, GlobalName} -> GlobalName
				end;
			true ->
				erlang:make_ref ()
		end,
		CoercedSpecification = {Identifier2, {Module, start_link, StartLinkArguments}, Restart, Shutdown, worker, dynamic},
		{ok, CoercedSpecification}
	catch throw : {error, Reason} -> {error, {invalid_child_specification, OriginalSpecification, Reason}} end;
	
coerce_child_specification ({supervisor, Configuration}) ->
	coerce_child_specification ({supervisor, noname, Configuration});
	
coerce_child_specification ({supervisor, QualifiedName, Configuration}) ->
	coerce_child_specification ({supervisor, QualifiedName, Configuration, []});
	
coerce_child_specification (OriginalSpecification = {supervisor, QualifiedName, Configuration, Options}) ->
	try
		ok = if
			is_list (Options) -> ok;
			true -> throw ({error, {invalid_options, Options, invalid_term}})
		end,
		DefaultOptions = [{identifier, undefined}, {restart, permanent}, {shutdown, infinity}],
		FinalOptions = Options ++ DefaultOptions,
		{Identifier1, Restart, Shutdown} = case lists:sort (proplists:get_keys (FinalOptions)) of
			[identifier, restart, shutdown] ->
				{
					proplists:get_value (identifier, FinalOptions),
					proplists:get_value (restart, FinalOptions),
					proplists:get_value (shutdown, FinalOptions)};
			OptionKeys ->
				throw ({error, {invalid_options, Options, {invalid_option_keys, lists:subtract (OptionKeys, proplists:get_keys (DefaultOptions))}}})
		end,
		StartLinkArguments = if
			(QualifiedName =:= noname) -> [Configuration];
			true -> [QualifiedName, Configuration]
		end,
		Identifier2 = if
			(Identifier1 =/= undefined) -> Identifier1;
			(QualifiedName =/= noname) ->
				case QualifiedName of
					{local, LocalName} -> LocalName;
					{global, GlobalName} -> GlobalName
				end;
			true ->
				erlang:make_ref ()
		end,
		CoercedSpecification = {Identifier2, {mosaic_supervisor_tools, start_link, StartLinkArguments}, Restart, Shutdown, supervisor, dynamic},
		{ok, CoercedSpecification}
	catch throw : {error, Reason} -> {error, {invalid_child_specification, OriginalSpecification, Reason}} end;
	
coerce_child_specification (Specification = {Identifier, OriginalStart, Restart, Shutdown, Type, Modules}) ->
	try
		CoercedStart = enforce_ok_1 (coerce_start_specification (OriginalStart)),
		ok = if
			((Restart =:= permanent) orelse (Restart =:= transient) orelse (Restart =:= temporary)) -> ok;
			true -> throw ({error, {invalid_restart, Restart}})
		end,
		ok = if
			(Shutdown =:= brutal_kill) -> ok;
			(is_integer (Shutdown) andalso (Shutdown > 0)) -> ok;
			(Shutdown =:= infinity) -> ok;
			true -> throw ({error, {invalid_shutdown, Shutdown}})
		end,
		ok = if
			((Type =:= worker) orelse (Type =:= supervisor)) -> ok;
			true -> throw ({error, {invalid_type, Type}})
		end,
		ok = if
			(Modules =:= dynamic) -> ok;
			is_list (Modules) ->
				ok = lists:foreach (
						fun (Module) when is_atom (Module) -> ok; (Module) -> throw ({error, {invalid_modules, Modules, {invalid_module, Module}}}) end,
						Modules),
				ok;
			true -> throw ({error, {invalid_modules, Modules, invalid_term}})
		end,
		{ok, {Identifier, CoercedStart, Restart, Shutdown, Type, Modules}}
	catch throw : {error, Reason} -> {error, {invalid_child_specification, Specification, Reason}} end;
	
coerce_child_specification (Specification) ->
	{error, {invalid_child_specification, Specification, invalid_term}}.


coerce_start_specification (Specification = {Module, Function, Arguments}) ->
	try
		ok = if
			is_atom (Module) -> ok;
			true -> throw ({error, {invalid_module, Module}})
		end,
		ok = if
			is_atom (Function) -> ok;
			true -> throw ({error, {invalid_function, Function}})
		end,
		ok = if
			is_list (Arguments) -> ok;
			true -> throw ({error, {invalid_arguments, Arguments}})
		end,
		ModuleLoaded = erlang:module_loaded (Module),
		ok = if
			ModuleLoaded -> ok;
			true ->
				case code:ensure_loaded (Module) of
					{module, Module} -> ok;
					{error, Reason1} -> throw ({error, {invalid_module, Module, Reason1}})
				end
		end,
		FunctionExported = erlang:function_exported (Module, Function, erlang:length (Arguments)),
		ok = if
			FunctionExported -> ok;
			true -> throw ({error, {invalid_function, Function, not_exported}})
		end,
		{ok, Specification}
	catch throw : {error, Reason} -> {error, {invalid_start_specification, Specification, Reason}} end;
	
coerce_start_specification (Specification) ->
	{error, {invalid_start_specification, Specification, invalid_term}}.
