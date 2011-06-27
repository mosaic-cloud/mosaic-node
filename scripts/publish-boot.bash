#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

echo "[ii] publishing ${_package_name}-boot..." >&2

test -e "${_outputs}/package-boot.tar.gz"

if test -e "${_package_afs}" ; then
	cp -T "${_outputs}/package-boot.tar.gz" "${_package_afs}/${_package_name}-boot-${_package_version}.tar.gz"
fi

ssh -T cook.mosaic.tartarus. <"${_outputs}/package-boot.tar.gz"

exit 0
