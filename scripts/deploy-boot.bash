#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

if test "${_mosaic_deploy_cook:-true}" == true ; then
	test -e "${_outputs}/package.tar.gz"
	ssh -T "${_package_cook}" <"${_outputs}/package-boot.tar.gz"
fi

exit 0
