#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_workbench}/scripts/package"
"${_workbench}/scripts/package-boot"

"${_workbench}/../mosaic-components-rabbitmq/scripts/package"
"${_workbench}/../mosaic-components-riak-kv/scripts/package"
"${_workbench}/../mosaic-components-httpg/scripts/package"

if test "${_mosaic_do_all_java:-false}" == true ; then
	"${_workbench}/../mosaic-java-components/components-container/scripts/package"
	"${_workbench}/../mosaic-java-platform/mosaic-mvn/mosaic-cloudlet/scripts/package"
	"${_workbench}/../mosaic-java-platform/mosaic-mvn/mosaic-driver/scripts/package"
fi

if test "${_mosaic_do_all_examples:-false}" == true ; then
	"${_workbench}/../mosaic-examples-realtime-feeds/backend/scripts/package"
	# "${_workbench}/../mosaic-examples-realtime-feeds-java/scripts/package"
fi

exit 0
