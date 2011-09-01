
{application, mosaic_node, [
	{description, "mOSAIC node"},
	{vsn, "1"},
	{applications, [kernel, stdlib, mosaic_tools, mosaic_harness, riak_core, mochiweb, webmachine]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_node_app, defaults}},
	{env, []}
]}.
