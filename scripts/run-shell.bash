#!/dev/null

if ! test "${#}" -eq 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_index="${1}"

test "${_index}" -ge 1
test "${_index}" -le 8

_erl_args+=(
		-sname "mosaic-shell-${_index}@{_erl_host}" -setcookie "${_erl_cookie}"
		-remsh "mosaic-cluster-${_index}@localhost"
)

#mkdir -p "/tmp/mosaic/cluster/${_index}"
#cd "/tmp/mosaic/cluster/${_index}"

exec env "${_erl_env[@]}" "${_erl}" "${_erl_args[@]}"
