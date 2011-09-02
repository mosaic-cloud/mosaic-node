#!/dev/null

if ! test "${#}" -eq 2 -o "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_fqdn="${mosaic_node_fqdn:-}"
_ip="${mosaic_node_ip:-}"

if test "${#}" -eq 0 ; then
	_index=0
	_scenario=boot
	_fqdn="${_fqdn:-mosaic-1.loopback.vnet}"
	_ip="${_ip:-127.0.155.1}"
	_erl_name="mosaic-node@${_fqdn}"
	_webmachine_port="$(( _erl_epmd_port + 1 ))"
	_riak_handoff_port="$(( _erl_epmd_port + 2 ))"
else
	_index="${1}"
	_scenario="${2}"
	test "${_index}" -ge 1
	test "${_index}" -le 8
	_fqdn="${_fqdn:-mosaic-${_index}.loopback.vnet}"
	_ip="${_ip:-127.0.155.${_index}}"
	_erl_name="mosaic-node-${_index}@${_fqdn}"
	_webmachine_port="$(( _erl_epmd_port + 1 + (_index - 1) * 2 + 0 ))"
	_riak_handoff_port="$(( _erl_epmd_port + 1 + (_index - 1) * 2 + 1 ))"
fi

if test -n "${mosaic_node_temporary:-}" ; then
	_tmp="${mosaic_node_temporary}"
elif test -n "${mosaic_temporary:-}" ; then
	_tmp="${mosaic_temporary}/node/${_index}"
else
	_tmp="/tmp/mosaic/node/${_index}"
fi

if test -n "${mosaic_log:-}" ; then
	_log="${mosaic_log}"
else
	_log=''
fi

_erl_args+=(
		-noinput -noshell
		-name "${_erl_name}" -setcookie "${_erl_cookie}"
		-boot start_sasl
		-config "${_erl_libs}/mosaic_node/priv/mosaic_node.config"
		-mosaic_node tests_scenario "'${_scenario}'"
		-mosaic_node webmachine_listen "{\"${_ip}\", ${_webmachine_port}}"
		-mosaic_node node_fqdn "\"${_fqdn}\""
		-mosaic_node node_ip "\"${_ip}\""
		-riak_core handoff_ip "\"${_ip}\""
		-riak_core handoff_port "${_riak_handoff_port}"
		-run mosaic_node_tests test
)
_erl_env+=(
		mosaic_node_fqdn="${_fqdn}"
		mosaic_node_ip="${_ip}"
)

if test -n "${_workbench:-}" ; then
	_erl_env+=(
			_mosaic_workbench="${_workbench}"
	)
fi

if test -n "${_log}" -a -f "${_log}" ; then
	_erl_env+=(
			mosaic_node_log="${_log}"
	)
fi

mkdir -p "${_tmp}"
cd "${_tmp}"

exec env "${_erl_env[@]}" "${_erl}" "${_erl_args[@]}"
