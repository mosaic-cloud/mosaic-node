#!/dev/null

if ! test "${#}" -eq 2 -o "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_fqdn="${mosaic_node_fqdn:-}"
_fqdn_app="${mosaic_application_fqdn:-}"
_ip="${mosaic_node_ip:-}"

_index="${1:-0}"
_script="${2:-boot}"

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

if test -n "${mosaic_node_management_port:-}" ; then
	_webmachine_port="${mosaic_node_management_port}"
fi
if test -n "${mosaic_node_handoff_port:-}" ; then
	_riak_handoff_port="${mosaic_node_handoff_port}"
fi

if test -n "${mosaic_node_wui_ip:-}" ; then
	_wui_ip="${mosaic_node_wui_ip}"
fi
if test -n "${mosaic_node_wui_port:-}" ; then
	_wui_port="${mosaic_node_wui_port}"
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
	_log_to_pipe=false
else
	_log="${_tmp}/log.txt"
	_log_to_pipe=true
fi

_erl_args+=(
		-noinput -noshell
		-name "${_erl_name}" -setcookie "${_erl_cookie}"
		-boot start_sasl
		-config "${_erl_libs}/mosaic_node/priv/mosaic_node.config"
		-mosaic_node script "'${_script}'"
		-mosaic_node webmachine_address "{\"${_ip}\", ${_webmachine_port}}"
		-mosaic_node discovery_agent_udp_address "{\"${_discovery_mcast_ip}\", ${_discovery_port}}"
		-mosaic_node discovery_agent_tcp_address "{\"${_discovery_domain}\", \"${_ip}\", ${_discovery_port}}"
		-mosaic_node wui_address "{\"${_wui_ip}\", ${_wui_port}}"
		-mosaic_node node_fqdn "\"${_fqdn}\""
		-mosaic_node node_ip "\"${_ip}\""
		-riak_core handoff_ip "\"${_ip}\""
		-riak_core handoff_port "${_riak_handoff_port}"
		-run mosaic_node_scripts start
)
_erl_env+=(
		mosaic_node_fqdn="${_fqdn}"
		mosaic_node_ip="${_ip}"
)

if test -n "${_log}" -a -f "${_log}" ; then
	_erl_env+=(
			mosaic_node_log="${_log}"
	)
fi

mkdir -p -- "${_tmp}"
cd -- "${_tmp}"

if test "${_log_to_pipe}" == false ; then
	exec </dev/null >/dev/null 2>|"${_log}" 1>&2
else
	if test ! -e "${_tmp}/log.pipe" ; then
		mkfifo "${_tmp}/log.pipe"
	fi
	tee -a /dev/stderr <"${_tmp}/log.pipe" >|"${_log}" &
	exec </dev/null >/dev/null 2>|"${_tmp}/log.pipe" 1>&2
fi

exec env "${_erl_env[@]}" "${_erl_bin}" "${_erl_args[@]}"
