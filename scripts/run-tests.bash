#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_module="${1}"
_webmachine_port="$(( _erl_epmd_port + 1 ))"
_riak_handoff_port="$(( _erl_epmd_port + 2 ))"

if test -n "${mosaic_node_temporary:-}" ; then
	_tmp="${mosaic_node_temporary}/cluster/0"
else
	_tmp="/tmp/mosaic/cluster/0"
fi

_erl_args+=(
		-noinput -noshell
		-name mosaic-cluster-0@mosaic-0.loopback.vnet
		-setcookie "${_erl_cookie}"
		-boot start_sasl
		-config "${_outputs}/erlang/applications/mosaic_cluster/priv/mosaic_cluster.config"
		-mosaic_cluster webmachine_listen "{\"127.0.0.1\", ${_webmachine_port}}"
		-riak_core handoff_ip "\"127.0.0.1\""
		-riak_core handoff_port "${_riak_handoff_port}"
		-run "${_module}" test
)
_erl_env+=(
		_mosaic_cluster_workbench="${_workbench}"
)

mkdir -p "${_tmp}"
cd "${_tmp}"

exec env "${_erl_env[@]}" "${_erl}" "${_erl_args[@]}"
