#!/dev/null

if ! test "${#}" -eq 0 -o "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_index="${1:-0}"

test "${_index}" -ge 0 -a "${_index}" -le 8

if test "${_index}" -ge 1 ; then
	_suffix="-${_index}"
else
	_suffix=''
fi

_erl_args+=(
		-name "mosaic-node-shell${_suffix}@mosaic${_suffix}.loopback"
		-remsh "mosaic-node${_suffix}@mosaic${_suffix}.loopback"
		-setcookie "${_erl_cookie}"
)

exec env "${_erl_env[@]}" "${_erl_bin}" "${_erl_args[@]}"
