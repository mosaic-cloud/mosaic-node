
-module (mosaic_process_configurator).


-export ([configure/4, configure/5, register/3, register/4, unregister/2, unregister/3]).


configure (Type, Identifier, ConfigurationEncoding, ConfigurationContent) ->
	configure (mosaic_process_configurator, Type, Identifier, ConfigurationEncoding, ConfigurationContent).

configure (Configurator, Type, Identifier, ConfigurationEncoding, ConfigurationContent)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding) ->
	gen_server:call (Configurator, {mosaic_process_configurator, configure, Type, Identifier, ConfigurationEncoding, ConfigurationContent}).


register (Type, ConfigurationEncoding, Function) ->
	register (mosaic_process_configurator, Type, ConfigurationEncoding, Function).

register (Configurator, Type, ConfigurationEncoding, {Module, Function, FunctionExtraArguments})
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding),
				is_atom (Module), is_atom (Function) ->
	gen_server:call (Configurator, {mosaic_process_configurator, register, Type, ConfigurationEncoding, {Module, Function, FunctionExtraArguments}});
	
register (Configurator, Type, ConfigurationEncoding, Function)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding), is_function (Function, 4) ->
	gen_server:call (Configurator, {mosaic_process_configurator, register, Type, ConfigurationEncoding, Function}).


unregister (Type, ConfigurationEncoding) ->
	unregister (mosaic_process_configurator, Type, ConfigurationEncoding).

unregister (Configurator, Type, ConfigurationEncoding)
		when (is_pid (Configurator) orelse is_atom (Configurator)), is_atom (Type), is_atom (ConfigurationEncoding) ->
	gen_server:call (Configurator, {mosaic_process_configurator, unregister, Type, ConfigurationEncoding}).
