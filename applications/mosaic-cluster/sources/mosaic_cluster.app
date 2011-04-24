
{application, mosaic_cluster, [
	{description, "mOSAIC cluster manager"},
	{vsn, "1"},
	{applications, [kernel, stdlib, riak_core]},
	{modules, []},
	{registered, [mosaic_cluster_sup]},
	{mod, {mosaic_cluster_app, defaults}},
	{env, []}
]}.
