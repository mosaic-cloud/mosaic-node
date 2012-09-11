#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_scenario="${1}"
_fqdn="${_fqdn:-mosaic.loopback}"
_ip="${_ip:-127.0.155.0}"
_erl_name="mosaic-node@${_fqdn}"
_webmachine_port="$(( _erl_epmd_port + 1 ))"
_riak_handoff_port="$(( _erl_epmd_port + 2 ))"
_discovery_port="$(( _erl_epmd_port - 1 ))"
_discovery_mcast_ip="224.0.0.1"
_discovery_domain="${_fqdn_app:-}"
_wui_ip="${_ip}"
_wui_port="$(( _erl_epmd_port + 3 ))"


if test -n "${mosaic_node_temporary:-}" ; then
	_tmp="${mosaic_node_temporary}"
elif test -n "${mosaic_temporary:-}" ; then
	_tmp="${mosaic_temporary}/node/0"
else
	_tmp="/tmp/mosaic/node/0"
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

if test -n "${_repositories:-}" ; then
	_erl_env+=(
			_mosaic_repositories="${_repositories}"
			PATH="$( find "${_repositories}"/*/ -path "${_repositories}"'/mosaic-*/.outputs/package/bin' -exec readlink -e {} \; | tr '\n' ':' ):${_outputs}/gcc/applications-elf:${_PATH}"
	)
fi

mkdir -p -- "${_tmp}"
cd -- "${_tmp}"

exec env "${_erl_env[@]}" "${_erl_bin}" "${_erl_args[@]}"
