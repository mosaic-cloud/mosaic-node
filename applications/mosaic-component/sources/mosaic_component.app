
{application, mosaic_component, [
	{description, "mOSAIC component"},
	{vsn, "1"},
	{applications, [kernel, stdlib, mosaic_tools, mosaic_harness]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_component_app, defaults}},
	{env, [
		{callbacks, undefined},
		{harness_input_descriptor, undefined},
		{harness_output_descriptor, undefined}]}
]}.
