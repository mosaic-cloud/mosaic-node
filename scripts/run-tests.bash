#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_index=0
_module="${1}"

_erl_args=(
		"${_erl_args[@]}"
		-noinput -noshell
		-sname "mosaic-cluster-${_index}@${_erl_host}" -setcookie "${_erl_cookie}"
		-boot start_sasl
		-config "${_outputs}/erlang/applications/mosaic_cluster/priv/mosaic_cluster.config"
		-mosaic_cluster tests_scenario "${_scenario}"
		-mosaic_cluster webmachine_listen "{\"127.0.0.1\", $(( _erl_epmd_port + 1 + (_index - 1) * 2 + 0 ))}"
		-riak_core handoff_port "$(( _erl_epmd_port + 1 + (_index - 1) * 2 + 1 ))"
		-run "${_module}" test
)
_erl_env=(
		ERL_EPMD_PORT="${_erl_epmd_port}"
)

#mkdir -p "/tmp/mosaic/cluster/0"
#cd "/tmp/mosaic/cluster/0"

exec env "${_erl_env[@]}" "${_erl}" "${_erl_args[@]}"
