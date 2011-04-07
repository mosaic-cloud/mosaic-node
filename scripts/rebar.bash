#!/dev/null

if test "${#}" -eq 0 ; then
	exec "${_rebar}" "${_rebar_args[@]}"
else
	exec "${_rebar}" "${_rebar_args[@]}" "${@}"
fi
