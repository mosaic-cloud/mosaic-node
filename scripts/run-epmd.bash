#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_epmd_argv=(
	"${_epmd}"
		"${_epmd_args[@]}"
)

exec "${_epmd_argv[@]}"
