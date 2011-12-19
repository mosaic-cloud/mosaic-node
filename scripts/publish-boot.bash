#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

test -e "${_outputs}/package-boot.tar.gz"

if test -e "${_package_afs}" ; then
	cp -T "${_outputs}/package-boot.tar.gz" "${_package_afs}/${_package_name}-boot-${_package_version}.tar.gz"
fi

exec ssh -T cook@agent1.builder.mosaic.ieat.ro. <"${_outputs}/package-boot.tar.gz"
exit 1
