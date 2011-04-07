#!/dev/null

_scripts="$( readlink -e -- ./scripts || true )"
_tools="$( readlink -f -- ./.tools || true )"
_outputs="$( readlink -f -- ./.outputs || true )"

_PATH="${_tools}/bin:${PATH}"

_erl="$( PATH="${_PATH}" type -P -- erl || true )"
if test -z "${_erl}" ; then
	echo "[ww] missing \`erl\` (Erlang interpreter) executable in path: \`${_PATH}\`; ignoring!" >&2
	_erl=erl
fi

_epmd="$( PATH="${_PATH}" type -P -- epmd || true )"
if test -z "${_epmd}" ; then
	echo "[ww] missing \`epmd\` (Erlang Process Mapper Daemon) executable in path: \`${_PATH}\`; ignoring!" >&2
	_epmd=epmd
fi

_rebar="$( PATH="${_PATH}" type -P -- rebar || true )"
if test -z "${_rebar}" ; then
	echo "[ww] missing \`rebar\` (Erlang build tool) executable in path: \`${_PATH}\`; ignoring!" >&2
	_rebar=rebar
fi

_erl_libs=""
_erl_ebins=( "./applications/mosaic-cluster/ebin" )
for _erl_ebin in "${_outputs}/deps/"*"/ebin" ; do
	if test -e "${_erl_ebin}" ; then
		_erl_ebins+=( "${_erl_ebin}" )
	fi
done
_erl_epmd_port=31807
_erl_cookie=b895e1d3-b7fe-4524-9fc9-e0b2f488396e
_erl_args=(
	+Bd +Ww
	-env ERL_CRASH_DUMP /dev/null
	-env ERL_LIBS "${_erl_libs}"
	-env ERL_EPMD_PORT "${_erl_epmd_port}"
	-pa "${_erl_ebins[@]}"
)

_epmd_port="${_erl_epmd_port}"
_epmd_args=(
	-port "${_epmd_port}"
	-debug
)

_rebar_args=( -j 4 )
