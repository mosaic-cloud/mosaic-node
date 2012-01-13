#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_index="${1}"

test "${_index}" -ge 1
test "${_index}" -le 8

_erl_args+=(
		-name "mosaic-node-shell-${_index}@mosaic-0.loopback.vnet"
		-remsh "mosaic-node-${_index}@mosaic-${_index}.loopback.vnet"
		-setcookie "${_erl_cookie}"
)

exec env "${_erl_env[@]}" "${_erl_bin}" "${_erl_args[@]}"
