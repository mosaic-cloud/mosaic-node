
{application, mosaic_tools, [
	{description, "mOSAIC tools"},
	{vsn, "1"},
	{applications, [kernel, stdlib, crypto]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_dummy_app, defaults}},
	{env, []}
]}.
