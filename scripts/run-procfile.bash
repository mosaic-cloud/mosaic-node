#!/dev/null

if ! test "${#}" -ge 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_procfile="${1}"
shift 1

test -e "${_procfile}"


_goreman_bin="$( PATH="${_PATH}" type -P -- goreman || true )"
if test -z "${_goreman_bin}" ; then
	echo "[ee] missing \`goreman\` (Foreman clone) executable in path: \`${_PATH}\`; ignoring!" >&2
	exit 1
fi

_goreman_args=()
_goreman_env=(
		"${_generic_env[@]}"
)

_goreman_args+=(
		-f "${_procfile}" start
)
if test "${#}" -ne 0 ; then
	_goreman_args+=( "${@}" )
fi


exec env "${_goreman_env[@]}" "${_goreman_bin}" "${_goreman_args[@]}"
