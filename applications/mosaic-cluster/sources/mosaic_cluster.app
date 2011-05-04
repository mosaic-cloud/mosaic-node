
{application, mosaic_cluster, [
	{description, "mOSAIC cluster manager"},
	{vsn, "1"},
	{applications, [kernel, stdlib, riak_core, mochiweb, webmachine]},
	{modules, []},
	{registered, [mosaic_sup]},
	{mod, {mosaic_cluster_app, defaults}},
	{env, []}
]}.
