
{application, mosaic_harness, [
	{description, "mOSAIC harness"},
	{vsn, "1"},
	{applications, [kernel, stdlib, mosaic_tools]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_dummy_app, defaults}},
	{env, []}
]}.
