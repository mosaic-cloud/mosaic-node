#!/dev/null

_scripts="$( readlink -e -- ./scripts || true )"
_tools="$( readlink -f -- ./.tools || true )"
_outputs="$( readlink -f -- ./.outputs || true )"
_make=ninja

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

_vbs="$( PATH="${_PATH}" type -P -- vbs || true )"
if test -z "${_vbs}" ; then
	echo "[ww] missing \`vbs\` (Volution Build System tool) executable in path: \`${_PATH}\`; ignoring!" >&2
	_vbs=vbs
fi

_ninja="$( PATH="${_PATH}" which -- ninja 2>/dev/null || true )"
if test -z "${_ninja}" ; then
	echo "[ww] missing \`ninja\` (Ninja build tool) executable in path: \`${_PATH}\`; ignoring!" >&2
	_ninja=ninja
fi

_erl_libs="${_outputs}/erlang/applications"
_erl_cookie=b895e1d3-b7fe-4524-9fc9-e0b2f488396e
_erl_epmd_port=31807
_erl_args=(
	+Bd +Ww
	-env ERL_CRASH_DUMP /dev/null
	-env ERL_LIBS "${_erl_libs}"
	-env ERL_EPMD_PORT "${_erl_epmd_port}"
)

_epmd_port="${_erl_epmd_port}"
_epmd_args=(
	-port "${_epmd_port}"
	-debug
)

_ninja_file="${_outputs}/.make.ninja"
_ninja_args=(
	-f "${_ninja_file}"
)
