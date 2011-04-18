#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_index="${1}"

test "${_index}" -ge 1
test "${_index}" -le 8

if ! test -e "/tmp/mosaic-cluster/node-${_index}-ring" ; then
	mkdir -p "/tmp/mosaic-cluster/node-${_index}-ring"
fi

_erl_argv=(
	"${_erl}"
		"${_erl_args[@]}"
		# -noshell -noinput
		-sname "mosaic-cluster-${_index}" -setcookie "${_erl_cookie}"
		-config "./applications/mosaic-cluster/priv/mosaic_cluster.config"
		-riak_core ring_state_dir "\"/tmp/mosaic-cluster/${_index}/ring\""
		-riak_core handoff_port "$(( _erl_epmd_port + _index ))"
		-run mosaic_cluster_app boot
)

ERL_EPMD_PORT="${_erl_epmd_port}" exec "${_erl_argv[@]}"
