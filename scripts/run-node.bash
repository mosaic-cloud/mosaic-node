#!/dev/null

if ! test "${#}" -eq 2 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_scenario="${1}"
_index="${2}"

test "${_index}" -ge 1
test "${_index}" -le 8

if test -e "/tmp/mosaic/cluster/${_index}" ; then
	rm -R "/tmp/mosaic/cluster/${_index}"
fi

mkdir -p "/tmp/mosaic/cluster/${_index}/ring"

if test "${_scenario}" != "shell" ; then
	_erl_argv=(
		"${_erl}"
			"${_erl_args[@]}"
			-noinput -noshell
			-sname "mosaic-cluster-${_index}@${_erl_host}" -setcookie "${_erl_cookie}"
			-boot start_sasl
			-config "${_outputs}/erlang/applications/mosaic_cluster/priv/mosaic_cluster.config"
			-mosaic_cluster tests_scenario "${_scenario}"
			-mosaic_cluster webmachine_listen "{\"127.0.0.1\", $(( _erl_epmd_port + 1 + (_index - 1) * 2 + 0 ))}"
			-riak_core ring_state_dir "\"/tmp/mosaic/cluster/${_index}/ring\""
			-riak_core handoff_port "$(( _erl_epmd_port + 1 + (_index - 1) * 2 + 1 ))"
			-run mosaic_cluster_tests test
	)
	_erl_env=(
		ERL_EPMD_PORT="${_erl_epmd_port}"
	)
else
	_erl_argv=(
		"${_erl}"
			"${_erl_args[@]}"
			-sname "mosaic-shell-${_index}@{_erl_host}" -setcookie "${_erl_cookie}"
			-remsh "mosaic-cluster-${_index}@localhost"
	)
	_erl_env=(
		ERL_EPMD_PORT="${_erl_epmd_port}"
	)
fi

exec env "${_erl_env[@]}" "${_erl_argv[@]}"
