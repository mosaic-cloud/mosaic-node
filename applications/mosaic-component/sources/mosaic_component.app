
{application, mosaic_component, [
	{description, "mOSAIC component"},
	{vsn, "1"},
	{applications, [kernel, stdlib, mosaic_tools, mosaic_harness]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_component_app, defaults}},
	{env, [
		{controller, undefined},
		{identifier, undefined},
		{handler, undefined}]},
]}.
