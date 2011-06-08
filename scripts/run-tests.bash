#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_module="${1}"

if test -e "/tmp/mosaic/cluster/0" ; then
	rm -R "/tmp/mosaic/cluster/0"
fi

mkdir -p "/tmp/mosaic/cluster/0/ring"

_erl_argv=(
	"${_erl}"
		"${_erl_args[@]}"
		-noshell -noinput
		-sname "mosaic-cluster-tests" -setcookie "${_erl_cookie}"
		-boot start_clean
		-config "${_outputs}/erlang/applications/mosaic_cluster/priv/mosaic_cluster.config"
		-mosaic_cluster webmachine_listen "{\"127.0.0.1\", $(( _erl_epmd_port + 1 + (9 - 1) * 2 + 0 ))}"
		-riak_core ring_state_dir "\"/tmp/mosaic/cluster/0/ring\""
		-riak_core handoff_port "$(( _erl_epmd_port + 1 + (9 - 1) * 2 + 1 ))"
		-run "${_module}" test
		-run init stop
)

ERL_EPMD_PORT="${_erl_epmd_port}" exec "${_erl_argv[@]}"
