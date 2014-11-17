#!/dev/null

if ! test "${#}" -eq 2 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_fqdn="${mosaic_node_fqdn:-}"
_fqdn_app="${mosaic_application_fqdn:-}"
_ip="${mosaic_node_ip:-}"

_index="${1}"
_scenario="${2}"

test "${_index}" -ge 0 -a "${_index}" -le 8

if test "${_index}" -ge 1 ; then
	_suffix="-${_index}"
else
	_suffix=''
fi

_fqdn="${_fqdn:-mosaic${_suffix}.loopback}"
_ip="${_ip:-127.0.155.${_index}}"
_erl_name="mosaic-node${_suffix}@${_fqdn}"
_webmachine_port="$(( _erl_epmd_port + 1 + _index * 10 + 0 ))"
_riak_handoff_port="$(( _erl_epmd_port + 1 + _index * 10 + 1 ))"
_discovery_port="$(( _erl_epmd_port - 1 ))"
_discovery_mcast_ip="224.0.0.1"
_discovery_domain="${_fqdn_app:-}"
_wui_ip="${_ip}"
_wui_port="$(( _erl_epmd_port + 1 + _index * 10 + 2 ))"

if test -n "${mosaic_node_temporary:-}" ; then
	_tmp="${mosaic_node_temporary}"
elif test -n "${mosaic_temporary:-}" ; then
	_tmp="${mosaic_temporary}/nodes/${_index}"
else
	_tmp="${TMPDIR:-/tmp}/mosaic/nodes/${_index}"
fi

_erl_args+=(
		-noinput -noshell
		-name "${_erl_name}" -setcookie "${_erl_cookie}"
		-boot start_sasl
		-config "${_erl_libs}/mosaic_node/priv/mosaic_node.config"
		-mosaic_node tests_scenario "'${_scenario}'"
		-mosaic_node webmachine_address "{\"${_ip}\", ${_webmachine_port}}"
		-mosaic_node discovery_agent_udp_address "{\"${_discovery_mcast_ip}\", ${_discovery_port}}"
		-mosaic_node discovery_agent_tcp_address "{\"${_discovery_domain}\", \"${_ip}\", ${_discovery_port}}"
		-mosaic_node wui_address "{\"${_wui_ip}\", ${_wui_port}}"
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

mkdir -p -- "${_tmp}"
cd -- "${_tmp}"

exec {_lock}<"${_tmp}"
if ! flock -x -n "${_lock}" ; then
	echo '[ee] failed to acquire lock; aborting!' >&2
	exit 1
fi

exec env -i "${_erl_env[@]}" "${_erl_bin}" "${_erl_args[@]}"

exit 1
