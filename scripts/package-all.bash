#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

"${_workbench}/scripts/package"
"${_workbench}/scripts/package-boot"

"${_workbench}/../mosaic-node-wui/scripts/package"

"${_workbench}/../mosaic-components-rabbitmq/scripts/package"
"${_workbench}/../mosaic-components-riak-kv/scripts/package"
"${_workbench}/../mosaic-components-httpg/scripts/package"

"${_workbench}/../mosaic-erlang-drivers/scripts/package"

if test "${_mosaic_do_all_java:-${_mosaic_do_all:-false}}" == true ; then
	"${_workbench}/../mosaic-java-platform/infrastructure/components-container/scripts/package"
	"${_workbench}/../mosaic-java-platform/cloudlets/scripts/package"
	"${_workbench}/../mosaic-java-platform/drivers/scripts/package"
fi

if test "${_mosaic_do_all_examples:-${_mosaic_do_all:-false}}" == true ; then
	"${_workbench}/../mosaic-examples-realtime-feeds/backend/scripts/package"
	"${_workbench}/../mosaic-examples-realtime-feeds-java/scripts/package"
fi

exit 0
