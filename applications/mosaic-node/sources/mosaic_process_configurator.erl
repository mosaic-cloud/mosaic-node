
-module (mosaic_process_configurator).


-export ([configure/5, configure/6, register/3, register/4, register/5, unregister/2, unregister/3, select/0, select/1]).


configure (Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent) ->
	configure (mosaic_process_configurator, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent).

configure (Configurator, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding),
				((Disposition =:= create) orelse (is_record (Disposition, migrate, 2) andalso ((element (2, Disposition) =:= source) orelse (element (2, Disposition) =:= target)))) ->
	gen_server:call (Configurator, {mosaic_process_configurator, configure, Type, Disposition, Identifier, ConfigurationEncoding, ConfigurationContent}).


register (Type, ConfigurationEncoding, Function) ->
	register (mosaic_process_configurator, Type, ConfigurationEncoding, Function, undefined).

register (Type, ConfigurationEncoding, Function, Annotation) ->
	register (mosaic_process_configurator, Type, ConfigurationEncoding, Function, Annotation).

register (Configurator, Type, ConfigurationEncoding, {Module, Function, FunctionExtraArgument}, Annotation)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding),
				is_atom (Module), is_atom (Function) ->
	gen_server:call (Configurator, {mosaic_process_configurator, register, Type, ConfigurationEncoding, {Module, Function, FunctionExtraArgument}, Annotation});
	
register (Configurator, Type, ConfigurationEncoding, {Module, Function}, Annotation)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding),
				is_atom (Module), is_atom (Function) ->
	gen_server:call (Configurator, {mosaic_process_configurator, register, Type, ConfigurationEncoding, {Module, Function}, Annotation});
	
register (Configurator, Type, ConfigurationEncoding, Function, Annotation)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding), is_function (Function, 4) ->
	gen_server:call (Configurator, {mosaic_process_configurator, register, Type, ConfigurationEncoding, Function, Annotation}).


unregister (Type, ConfigurationEncoding) ->
	unregister (mosaic_process_configurator, Type, ConfigurationEncoding).

unregister (Configurator, Type, ConfigurationEncoding)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding) ->
	gen_server:call (Configurator, {mosaic_process_configurator, unregister, Type, ConfigurationEncoding}).


select () ->
	select (mosaic_process_configurator).

select (Configurator)
		when (is_pid (Configurator) orelse is_atom (Configurator)) ->
	gen_server:call (Configurator, {mosaic_process_configurator, select}).
